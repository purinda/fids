unit uSocket;

interface

uses
  Winsock;

implementation

function PortTCPIsOpen(dwPort : Word; ipAddressStr:string) : boolean;
var
  client : sockaddr_in;//sockaddr_in is used by Windows Sockets to specify a local or remote endpoint address
  sock, ret : Integer;
  wsdata : WSAData;
  isConnected : Boolean;
begin
    ret := WSAStartup($0002, wsdata);//initiates use of the Winsock
    if ret <> 0 then exit;
    try
      client.sin_family      := AF_INET;
      client.sin_port        := htons(dwPort);//htons converts a u_short from host to TCP/IP network byte order.
      client.sin_addr.s_addr := inet_addr(PAnsiChar(ipAddressStr)); //the inet_addr function converts a string containing an IPv4 dotted-decimal address into a proper address for the IN_ADDR structure.
      sock                   := socket(AF_INET, SOCK_STREAM, 0);//The socket function creates a socket
      isConnected := connect(sock,client,SizeOf(client)) = 0;//establishes a connection to a specified socket.

    finally
    WSACleanup; //terminates use of the Winsock
    end;

    Result := isConnected;
end;

end.
