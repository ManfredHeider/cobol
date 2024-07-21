      *****************************************************************
      *    COPY-MEMBER: DRVB1354                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0030                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.08.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-5133 !08.08.22! NEU                           ! KLEEMEIER *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1354 - ERMITTELN ZVZ-DATEN PER STICHTAG       *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1354.
          05 PI-DRVB1354-DATEN.
             10 PI-DRVB1354-MA-ST-NR        PIC S9(009) COMP.
             10 PI-DRVB1354-STICHTAG        PIC  X(010).
      *
       01 PO-DRVB1354.
          05 PO-DRVB1354-RC                 PIC  X(003).
             88 PO-DRVB1354-RC-OK                VALUE 'OK '.
             88 PO-DRVB1354-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB1354-DATEN.                                                 
             10 PO-DRVB1354-NI-VIVB0030.                                        
                15 NI-DRVB1354-MA-ST-NR            PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZV-NR               PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-DAT-VON         PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-LAST-UPDATE     PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-DAT-BIS         PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-VBG-AB          PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-VBG-VNR         PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-ABSICHERG       PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-DVAG-SUMME      PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-VBG-BEITRAG     PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-CEN-AB          PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-CEN-VNR         PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-CEN-BETR        PIC S9(004) BINARY.          
                15 NI-DRVB1354-ZVZ-CEN-BEITRAG     PIC S9(004) BINARY.          
             10 PO-DRVB1354-DCL-VIVB0030.                                       
                15 PO-DRVB1354-MA-ST-NR            PIC S9(009) BINARY.          
                15 PO-DRVB1354-ZV-NR               PIC  X(001).                 
                15 PO-DRVB1354-ZVZ-DAT-VON         PIC  X(010).                 
                15 PO-DRVB1354-ZVZ-LAST-UPDATE     PIC  X(026).                 
                15 PO-DRVB1354-ZVZ-DAT-BIS         PIC  X(010).                 
                15 PO-DRVB1354-ZVZ-VBG-AB          PIC  X(010).                 
                15 PO-DRVB1354-ZVZ-VBG-VNR         PIC  X(015).                 
                15 PO-DRVB1354-ZVZ-ABSICHERG                                    
                                            PIC S9(009)V9(002) COMP-3.          
                15 PO-DRVB1354-ZVZ-DVAG-SUMME                                   
                                            PIC S9(009)V9(002) COMP-3.          
                15 PO-DRVB1354-ZVZ-VBG-BEITRAG                                  
                                            PIC S9(009)V9(002) COMP-3.          
                15 PO-DRVB1354-ZVZ-CEN-AB          PIC  X(010).                 
                15 PO-DRVB1354-ZVZ-CEN-VNR         PIC  X(015).                 
                15 PO-DRVB1354-ZVZ-CEN-BETR PIC S9(009)V9(002) COMP-3.          
                15 PO-DRVB1354-ZVZ-CEN-BEITRAG                                  
                                            PIC S9(009)V9(002) COMP-3.          
      ****
      **** ENDE COPY DRVB1354 ****************************************
      ****
