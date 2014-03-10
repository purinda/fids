unit uThreadControl;

interface

uses
  Generics.Collections, Generics.Defaults, SyncObjs, Classes, Contnrs,
  uPacket, uGT, uPoller;

const
    Infinite = $FFFFFFFF;

type
	aWaitResult = ( wrSignaled, wrTimeout, wrAbandoned, wrError, wrIOCompletion, wrShutDown ); // see TWaitResult
    aPriority = 0..10;

const
    MiddlePriority = High( aPriority ) div 2;  // background priority - no rush
    ReducedPriority = Low( aPriority ) + 3;
    ElevatedPriority = High( aPriority ) - 3;

type
	cThreadControl = class( TEvent )
    	private
            mNumWaiting : integer;
    	public
			constructor Create( InitialState : boolean );
            destructor  Destroy;  override;
        	function    WaitFor( Timeout : LongWord ) : aWaitResult;  reintroduce;
            procedure   Signal;
            class procedure   WakeAll;
            property    NumWaiting : integer  read mNumWaiting;
    	end;

    apEventHandler = procedure of object;

    cThread = class( TThread )
    	private
        	mOnExecute : apEventHandler;
            class var   mShutDown : boolean;
            class var   mThreadCount : integer;
            procedure   SetOnExec( eh : apEventHandler );
        protected
        	procedure   Execute;  override;
        public
            oQ : cLockedQueue;
            oEvent : cThreadControl;
        	constructor Create;
            class procedure   ShutDownAll;
        property    OnExecute : apEventHandler read mOnExecute write SetOnExec;
        class property ShutDown : boolean read mShutDown;
        class property ThreadCount : integer read mThreadCount;
    	end;

// _____________________________________________________________________________


    cThreadInQueue = class( TQueue )
        constructor Create( Input : TEvent );   overload;
        destructor  Destroy;   override;
        private
            oLock : TCriticalSection;
            mInput : TEvent;
        public
			procedure PostPointer( pm : pointer );    // untyped overload
			function  ReadPointer() : pointer;        // untyped overload
			function  Count() : int;

	    end;

    cWorkList = class;    cCPUBackgroundThread = class;

    aJobProc = reference to procedure;
    aBackgroundProc = procedure ( bt : cCPUBackgroundThread ) of object;

    cCPUWorkerThread = class( TThread )
    	constructor Create( workL : cWorkList );
    	private
        	mWorkL : cWorkList;
        protected
        	procedure Execute;  override;
    	end;

    cCPUBackgroundThread = class( TThread )
    	constructor Create( workL : cWorkList; work : aBackgroundProc; ProcPriority : int = MiddlePriority );
    	private
        	mWorkL : cWorkList;
    		mJob: aBackgroundProc;
    		mPriority: Integer;
            oQ : cThreadInQueue;
            oInput : TEvent;
            function  GetShutdown : boolean;
        protected
        	procedure Execute;  override;
        public
        	function  Read( timeout : int ) : pointer;
            procedure Post( pkt : pointer );
            property  Shutdown : boolean  read GetShutdown;
    	end;

    aParallelForProc = procedure( first, last : int ) of object;

    cParallelForThread = class( TThread )
    	constructor Create( first, last : int; ParallelForProc : aParallelForProc; WorkL : cWorkList );
    	private
            mParallelForProc : aParallelForProc;
            mFirst, mLast : int;
        	mWorkL : cWorkList;
        protected
        	procedure Execute;  override;
    	end;



    aJob = record
        Proc : aJobProc;
        Priority : int;
    	end;
    apJob = ^ aJob;

	cWorkList = class( TInterfacedObject, IComparer<apJob> )
    	constructor Create;
        destructor  Destroy;  override;
        private
            mJobs : TList< apJob >;
            oJobsLock : TCriticalSection;
            oComplete : TEvent;
            mNCores : int;
            mShutDown : boolean;
            mThreadCount, mBackgroundThreadCount : integer;
            mCurrentHighestPriority : int;
            mOnCompletion : aOnTimeOutProc;
            procedure SignalCompletion;
            function  Compare( const Left, Right : apJob ) : Integer;
            procedure SetOnCompletion( proc : aOnTimeOutProc );
			// procedure NewJob( ProcPriority : int );
    	public
        	procedure DoTransient( work : aJobProc; ProcPriority : int = MiddlePriority );
        	function  DoInBackground( work : aBackgroundProc; ProcPriority : int = MiddlePriority ) : cCPUBackgroundThread;
            procedure ParallelFor( first, last : int; work : aParallelForProc; threads : int = 0 );
            procedure WaitForCompletion( timeout : int = 0 );
            procedure Lock;
            procedure Unlock;
        	property  ThreadCount : integer read mThreadCount;
            property  CurrentHighestPriority : int read mCurrentHighestPriority;
            property  OnCompletion : aOnTimeOutProc  read mOnCompletion  write SetOnCompletion;
            property  ShutDown : boolean  read mShutDown;
    	end;


function  GetNCores : int;


var
	CPUs : cWorkList = nil;


implementation

uses   Windows, SysUtils;

var
    AllThreadControls : TList< cThreadControl > = nil;


function  GetNCores : int;

    var
	    processAffinityMask, systemAffinityMask : DWORD;
	begin
    GetProcessAffinityMask( GetCurrentProcess, processAffinityMask, systemAffinityMask );
    result := 0;
    while processAffinityMask <> 0 do  begin
    	if Odd( processAffinityMask ) then  Inc( result );
    	processAffinityMask := processAffinityMask shr 1;
    	end;
    end;


constructor cCPUWorkerThread.Create( workL : cWorkList );

    begin
    mWorkL := workL;
    InterlockedIncrement( mWorkL.mThreadCount );    // pre-emptive to avoid excess threads
    inherited Create( false );
    end;


procedure cCPUWorkerThread.Execute;  // override;

    var
    	jp : apJob;
	begin
    FreeOnTerminate := true;

    while not mWorkL.mShutDown do  begin
        mWorkL.oJobsLock.Acquire;
        try  begin            // find hightest priority waiting job
            if mWorkL.mJobs.Count > 0 then  begin  // deal them from the top - top is highest priority
                jp := mWorkL.mJobs[ 0 ];
                mWorkL.mJobs.Delete( 0 );
                end
            else jp := nil;
            end;
        finally
            mWorkL.oJobsLock.Release;
            end;
        if jp <> nil then  begin
        	if jp.Priority <= ReducedPriority then  Priority := tpLower
            else if jp.Priority >= ElevatedPriority then  Priority := tpHigher
            else  Priority := tpNormal;
            try  jp.Proc();      // do the job
            finally Dispose( jp );  end; // toss the record
            end
        else  break;

	    end;  // while work to do

    mWorkL.SignalCompletion;
    end;


constructor cParallelForThread. Create( first, last : int; ParallelForProc : aParallelForProc; WorkL : cWorkList );

	begin
    mFirst := first;
    mLast := last;
    mParallelForProc := ParallelForProc;
    mWorkL := WorkL;
    InterlockedIncrement( mWorkL.mThreadCount );
    try    inherited Create( false );
    except InterlockedDecrement( mWorkL.mThreadCount );   end;   // undo inc on exception
    end;


procedure  cParallelForThread. Execute;

	begin
    FreeOnTerminate := true;
    try  begin
        if not mWorkL.mShutDown then  begin
            mParallelForProc( mFirst, mLast );
            end;
	    end;
    finally
	    mWorkL.SignalCompletion;
        end;
    end;


procedure cWorkList. ParallelFor( first, last : int; work : aParallelForProc; threads : int = 0 );

    var
    	t, f, l, n : int;
	begin
    if threads = 0 then  threads := GetNCores;
    n := ( last - first + threads - 1  ) div threads;
    f := first;
    oComplete.ResetEvent;
    for t := 1 to threads do  begin
        if t < threads then  l := f + n - 1  else  l := last;
        cParallelForThread.Create( f, l, work, self );
        f := f + n;
    	end;
    end;


constructor cWorkList.Create;

	begin
    oJobsLock := TCriticalSection.Create;
    oComplete := TEvent.Create( nil, false, false, 'For Completion', false );
    mJobs := TList< apJob >.Create;
    mNCores := GetNCores;
    mCurrentHighestPriority := -1;
    end;


destructor  cWorkList.Destroy;

	begin
    mShutDown := true;    // tell threads to exit
    while mThreadCount > 0 do  Sleep( 10 );   // wait for all threads to exit
    while mBackgroundThreadCount > 0 do  Sleep( 10 );   // wait for all threads to exit
    FreeAndNil( oJobsLock );
    FreeAndNil( oComplete );
    end;


procedure cWorkList.Lock;

	begin
    oJobsLock.Acquire;
    end;


procedure cWorkList.Unlock;

	begin
    oJobsLock.Release;
    end;


procedure cWorkList.SignalCompletion;

	begin
    Lock;
    try  begin
        Dec( mThreadCount );
        if ( mThreadCount = 0 ) and Assigned( mOnCompletion ) then  begin
        	Poller.DoInMainThread( mOnCompletion );
        	oComplete.SetEvent;
        	end;
        end;
    finally
        Unlock;
        end;
    end;


function  cWorkList.Compare( const Left, Right : apJob ) : Integer;

	begin       // descending order of priority
    if left.Priority > right.Priority then  result := -1
    else if left.Priority < right.Priority then  result := 1
    else result := 0;
    end;


procedure cWorkList.SetOnCompletion( proc : aOnTimeOutProc );

	begin
    mOnCompletion := proc;
    oComplete.ResetEvent;
    end;


procedure cWorkList.DoTransient( work : aJobProc; ProcPriority : int = MiddlePriority );

    var
    	jp : apJob;
        doNew : boolean;
    label
        done;
	begin
    oJobsLock.Acquire;
    try  begin
        if mJobs.Count > 0 then  begin
        	doNew := ( ProcPriority > mJobs[ 0 ].Priority ) or ( mThreadCount < mNCores );   // new higher priority or idle cores
        	end
        else  doNew := true;

        New( jp );
        jp.Proc := work;
        jp.Priority := ProcPriority;
        mJobs.Add( jp );
        mJobs.Sort;  // keep in priority order
    	end;

    finally
    	oJobsLock.Release;
	    end;
    if doNew then  cCPUWorkerThread.Create( self );
    end;


constructor cThreadInQueue.  Create( Input : TEvent );   // todo generic typed thread

	begin
    mInput := Input;
    oLock := TCriticalSection.Create;
    inherited Create;
	end;


destructor  cThreadInQueue.  Destroy;  // override;

	begin
    mInput.SetEvent;  Sleep( 1 );  // in case reader thread is waiting to see shut down
    FreeAndNil( oLock );
    inherited  Destroy;
    end;


procedure  cThreadInQueue.PostPointer( pm : pointer ); // typically reader thread posting incoming to main thread

	begin
	oLock.Acquire;
	Push( pm );
	oLock.Release;
    mInput.SetEvent;  // wake the reader thread
	end;


function  cThreadInQueue.ReadPointer() : pointer;   // typically main thread reading incoming

	begin
	oLock.Acquire;
	result := Pop();
	oLock.Release;
	end;


function  cThreadInQueue.Count() : int;

	begin
	result := inherited Count();    // assuming count is atomic
	end;


constructor cCPUBackgroundThread.  Create( workL : cWorkList; work : aBackgroundProc; ProcPriority : int = MiddlePriority );

	begin
    mWorkL := workL;
    mJob := work;
    mPriority := ProcPriority;
    oInput := TEvent.Create( nil, false, false, 'Input' );
    oQ := cThreadInQueue.Create( oInput );

    InterlockedIncrement( mWorkL.mBackgroundThreadCount );
    try    inherited Create( false );
    except InterlockedDecrement( mWorkL.mBackgroundThreadCount );   end;   // undo inc on exception
    end;


procedure  cCPUBackgroundThread.  Execute;

	begin
    FreeOnTerminate := true;
    if mPriority <= MiddlePriority - 2 then  Priority := tpLower
    else if mPriority >= ElevatedPriority then  Priority := tpHigher
    else  Priority := tpNormal;
    try  begin
        if not mWorkL.mShutDown then  begin
            try  mJob( self );
            except  ;  end;
            end;
	    end;
    finally
	    InterlockedDecrement( mWorkL.mBackgroundThreadCount );
        end;
	end;


function  cCPUBackgroundThread.  GetShutdown : boolean;

	begin
    result := mWorkL.mShutDown;
    end;


function  cCPUBackgroundThread.  Read( timeout : int ) : pointer;

	begin
    result := nil;
    if timeout = 0 then  begin
        if oQ.Count > 0 then  result := oQ.ReadPointer;
    	end
    else  begin
        oInput.WaitFor( timeout );
        result := oQ.ReadPointer;
    	end;
    end;


procedure cCPUBackgroundThread.  Post( pkt : pointer );

	begin
    oQ.PostPointer( pkt );
    end;


function cWorkList.  DoInBackground( work : aBackgroundProc; ProcPriority : int = MiddlePriority ) : cCPUBackgroundThread;

    var
    	th : cCPUBackgroundThread;
	begin
    th := cCPUBackgroundThread.Create( self, work, ProcPriority );
    result := th;
    end;


procedure cWorkList.  WaitForCompletion( timeout : int = 0 );

	begin
    if timeout > 0 then  oComplete.WaitFor( timeout )  // wait for end signal
    else   				 oComplete.WaitFor( Infinite );
    end;


// _______________________________ cThread _____________________________________

constructor cThread.Create;

	begin
    inherited Create( true );  // wait for onExecute
    end;


procedure   cThread.SetOnExec( eh : apEventHandler );

	begin
    mOnExecute := eh;
    Start;
    end;


procedure  cThread.Execute;

	begin
    InterlockedIncrement( mThreadCount );
    try  begin
    	try  begin
            repeat
                if Assigned( mOnExecute ) then  mOnExecute;
                until ShutDown;
	        end;
        except  // swallow any exceptions
        	end;
	    end;
    finally
        InterlockedDecrement( mThreadCount );
        end;
    end;


class procedure   cThread.ShutDownAll;

	begin
    cThread.mShutDown := true;
    while mThreadCount > 0 do  begin
        cThreadControl.WakeAll;
    	Sleep( 20 );
	    end;
    end;


constructor cThreadControl.Create( InitialState : boolean );

	begin
    if AllThreadControls = nil then  AllThreadControls := TList< cThreadControl >.Create;
    inherited Create( nil, false, InitialState, '', false );
    AllThreadControls.Add( self );
    end;


destructor  cThreadControl.Destroy;

	begin
    while mNumWaiting > 0 do  begin
    	SetEvent;
        Sleep( 10 );
	    end;
    AllThreadControls.Delete( AllThreadControls.IndexOf( self ) );
    end;


function    cThreadControl.WaitFor( Timeout : LongWord ) : aWaitResult;

	begin
    InterlockedIncrement( mNumWaiting );
    result := aWaitResult( inherited WaitFor( Timeout ) );
    InterlockedDecrement( mNumWaiting );
    if cThread.mShutDown then  begin
    	// result := wrShutDown;
        raise Exception.Create( 'ShutDown' );   // forceful exception termination of thread
	    end;
    end;


procedure   cThreadControl.Signal;

    begin
    SetEvent;
    end;


class procedure   cThreadControl.WakeAll;

	var
        tc : cThreadControl;
    begin
    if AllThreadControls <> nil then  begin
        for tc in AllThreadControls do  begin
            while tc.mNumWaiting > 0 do  begin
                tc.SetEvent;
                Sleep( 10 );
                end;
        	end;
    	end;
    end;


initialization
	CPUs := cWorkList.Create;

finalization
	CPUs.Free;  CPUs:= nil;    // don't use FreeAndNil - really NilAndFree - nfg

end.
