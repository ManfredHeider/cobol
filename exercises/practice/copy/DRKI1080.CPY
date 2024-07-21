      *****************************************************************
      *    COPY-MEMBER: DRKI1080                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1080                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  03.04.2003/16:52:18
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
      *    FšR DIE FUNKTION DRKI1080
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1080.
           05  PI-DRKI1080-KEY.


             10 PI-DRKI1080-FRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI1080-FRAPERSONENID   PIC X(13).
             

           05  PI-DRKI1080-DATEN.


             10 PI-DRKI1080-FRAANZDEPFREISTBET   PIC S9(4) COMP.
             10 PI-DRKI1080-N-FRNZDPFRSTBT   PIC S9(4) COMP.
             10 PI-DRKI1080-FRAFRBETRVERBRWKZ   PIC X(01).
             10 PI-DRKI1080-FRAFREIBETRAGABL   PIC X(10).
             10 PI-DRKI1080-FRAFREIBETRAGVERBR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1080-N-FRFRBTRGVRBR   PIC S9(4) COMP.
             10 PI-DRKI1080-FRAFREMDVERTGESNAM.
                15 PI-DRKI1080-FRFRMDVRTGSNM-LN   PIC S9(4) COMP.
                15 PI-DRKI1080-FRFRMDVRTGSNM-TXT   PIC X(25).
                
             10 PI-DRKI1080-FRAGEMFRBETRAGWKZ   PIC X(01).
             10 PI-DRKI1080-FRAGEMFREIBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1080-N-FRGMFRBTRG   PIC S9(4) COMP.
             10 PI-DRKI1080-FRAGUELTIGAB   PIC X(10).
             10 PI-DRKI1080-FRAPARTNERGESID   PIC S9(4) COMP.
             10 PI-DRKI1080-FRASTUECKZINS   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI1080-NI-FRASTUECKZINS   PIC S9(4) COMP.
             10 PI-DRKI1080-FRAZINSABSCHSTEUER   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1080-N-FRZNSBSCHSTR   PIC S9(4) COMP.
             10 PI-DRKI1080-LETZTEAENDERUNG   PIC X(26).
             
      *
       01  PO-DRKI1080.
           05  PO-DRKI1080-RC                 PIC  X(003).
               88 PO-DRKI1080-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1080-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1080-DATEN.
               10 PO-DRKI1080-PTRTABLENAME.                                     
                  49 PO-DRKI1080-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1080-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1080-PTRKEYDATA.                                       
                  49 PO-DRKI1080-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1080-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1080-PTRSQLDATA.                                       
                  49 PO-DRKI1080-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1080-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
