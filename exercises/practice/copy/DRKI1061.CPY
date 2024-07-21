      *****************************************************************
      *    COPY-MEMBER: DRKI1061                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1061                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  02.04.2003/14:18:14
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1061
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1061.
           05  PI-DRKI1061-KEY.


             10 PI-DRKI1061-HWELAUFENDENUMMER   PIC S9(9) COMP.
             10 PI-DRKI1061-HWEPERSONENID   PIC X(13).
             10 PI-DRKI1061-HWEPERSONENROLLE   PIC X(01).
             

           05  PI-DRKI1061-DATEN.


             10 PI-DRKI1061-HWEABSCHRFUERANL   PIC X(30).
             10 PI-DRKI1061-HWEBAUKINDERGELD   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1061-N-HWBKNDRGLD   PIC S9(4) COMP.
             10 PI-DRKI1061-HWEBAUKINDGELDSEIT   PIC X(10).
             10 PI-DRKI1061-HWEBAUKINDGWKZ   PIC X(01).
             10 PI-DRKI1061-HWEEIGENHEIMZULAGE   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1061-N-HWGNHMZLG   PIC S9(4) COMP.
             10 PI-DRKI1061-HWEEIGENTUMSEIT   PIC X(10).
             10 PI-DRKI1061-HWEEIGHEIMZULWKZ   PIC X(01).
             10 PI-DRKI1061-HWEEIGTUMARTID   PIC X(3).
             10 PI-DRKI1061-HWEEIGTUMNARTID   PIC X(3).
             10 PI-DRKI1061-HWEKAUFBAUKOSTEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1061-N-HWKFBKSTN   PIC S9(4) COMP.
             10 PI-DRKI1061-HWEKAUFBAUKOSTWKZ   PIC X(01).
             10 PI-DRKI1061-HWEMONERTRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1061-NI-HWEMONERTRAG   PIC S9(4) COMP.
             10 PI-DRKI1061-HWEMONERTRAGWKZ   PIC X(01).
             10 PI-DRKI1061-HWEMONNEBENKOSTEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1061-N-HWMNNBNKSTN   PIC S9(4) COMP.
             10 PI-DRKI1061-HWEMONNEBKOSTWKZ   PIC X(01).
             10 PI-DRKI1061-HWEOBJEKTWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1061-NI-HWEOBJEKTWERT   PIC S9(4) COMP.
             10 PI-DRKI1061-HWEOBJKETWERTWKZ   PIC X(01).
             10 PI-DRKI1061-HWESELBSTGENFLAE   PIC S9(4)V9(2) COMP-3.
             10 PI-DRKI1061-N-HWSLBSTGNFL   PIC S9(4) COMP.
             10 PI-DRKI1061-HWESONDAUSGABEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1061-NI-HWESONDAUSGABEN   PIC S9(4) COMP.
             10 PI-DRKI1061-HWESONDAUSGWKZ   PIC X(01).
             10 PI-DRKI1061-HWEWERBKOSTEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1061-NI-HWEWERBKOSTEN   PIC S9(4) COMP.
             10 PI-DRKI1061-HWEWERBKOSTENWKZ   PIC X(01).
             10 PI-DRKI1061-LETZTEAENDERUNG   PIC X(26).
             
      *
       01  PO-DRKI1061.
           05  PO-DRKI1061-RC                 PIC  X(003).
               88 PO-DRKI1061-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1061-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1061-DATEN.
               10 PO-DRKI1061-PTRTABLENAME.                                     
                  49 PO-DRKI1061-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1061-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1061-PTRKEYDATA.                                       
                  49 PO-DRKI1061-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1061-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1061-PTRSQLDATA.                                       
                  49 PO-DRKI1061-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1061-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
