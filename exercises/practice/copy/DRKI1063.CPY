      *****************************************************************
      *    COPY-MEMBER: DRKI1063                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1063                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  02.04.2003/14:00:18
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
      *    FšR DIE FUNKTION DRKI1063
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1063.
           05  PI-DRKI1063-KEY.


             10 PI-DRKI1063-HYPHWELFDNUMMER   PIC S9(9) COMP.
             10 PI-DRKI1063-HYPLAUFENDENUMMER   PIC S9(9) COMP.
             10 PI-DRKI1063-HYPPERSONENID   PIC X(13).
             10 PI-DRKI1063-HYPPERSONENROLLE   PIC X(01).
             

           05  PI-DRKI1063-DATEN.


             10 PI-DRKI1063-HYPANLAGEZIEL   PIC X(50).
             10 PI-DRKI1063-HYPBANKBLZ   PIC S9(9) COMP.
             10 PI-DRKI1063-NI-HYPBANKBLZ   PIC S9(4) COMP.
             10 PI-DRKI1063-HYPBANKBLZTEXT   PIC X(30).
             10 PI-DRKI1063-HYPDARLEHENSART   PIC S9(4) COMP.
             10 PI-DRKI1063-NI-HYPDARLEHENSART   PIC S9(4) COMP.
             10 PI-DRKI1063-HYPDARLEHENSSUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1063-N-HYPDRLHNSSMM   PIC S9(4) COMP.
             10 PI-DRKI1063-HYPDARLSUMMEWKZ   PIC X(01).
             10 PI-DRKI1063-HYPFESTGELEGTBIS   PIC X(10).
             10 PI-DRKI1063-HYPMONBELASTUNG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1063-NI-HYPMONBELASTUNG   PIC S9(4) COMP.
             10 PI-DRKI1063-HYPMONBELASTWKZ   PIC X(01).
             10 PI-DRKI1063-HYPNOTIZID   PIC X(13).
             10 PI-DRKI1063-HYPRESTSCHULD   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1063-NI-HYPRESTSCHULD   PIC S9(4) COMP.
             10 PI-DRKI1063-HYPRESTSCHULDWKZ   PIC X(01).
             10 PI-DRKI1063-HYPTILGUNG   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1063-NI-HYPTILGUNG   PIC S9(4) COMP.
             10 PI-DRKI1063-HYPTILGUNGSBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1063-N-HYPTLGNGSBTRG   PIC S9(4) COMP.
             10 PI-DRKI1063-HYPTILGUNGSBEITWKZ   PIC X(01).
             10 PI-DRKI1063-HYPZINSEN   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1063-NI-HYPZINSEN   PIC S9(4) COMP.
             10 PI-DRKI1063-LETZTEAENDERUNG   PIC X(26).
             
      *
       01  PO-DRKI1063.
           05  PO-DRKI1063-RC                 PIC  X(003).
               88 PO-DRKI1063-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1063-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1063-DATEN.
               10 PO-DRKI1063-PTRTABLENAME.                                     
                  49 PO-DRKI1063-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1063-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1063-PTRKEYDATA.                                       
                  49 PO-DRKI1063-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1063-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1063-PTRSQLDATA.                                       
                  49 PO-DRKI1063-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1063-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
