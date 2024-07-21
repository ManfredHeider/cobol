      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0073.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\WVVB0073.CPv  $
      *
      *   Rev 2.3   Mar 03 2000 14:07:34   T13AE28
      * 
      *
      *   Rev 2.2   Feb 21 2000 12:37:18   T13AE28
      * 
      *
      *   Rev 2.1   Feb 18 2000 15:34:08   T13AE28
      * 
      *
      *   Rev 2.0   Feb 18 2000 10:15:42   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************         
      *    COPY-MEMBER: WVVB0073                                      *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 *         
      *    DATUM:    22.04.1993                                       *         
      *    STAND:    22.04.1993                                       *         
      *    --------------------------------------------------------   *         
      *    VERSION:  001                                              *         
      *****************************************************************         
      *                                                                         
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VIVB0073                           *        
      ******************************************************************        
        05  WB-NI-VIVB0073-TAB.                                                 
           10 WB-NI-VIVB0073-ELE   PIC S9(04)  COMP   OCCURS  8  TIMES.         
        05  WB-NI-VIVB0073            REDEFINES   WB-NI-VIVB0073-TAB.           
           10 WB-NI-MA-ST-NR          PIC S9(04)  COMP.                         
           10 WB-NI-BA-DAT-VON        PIC S9(04)  COMP.                         
           10 WB-NI-BA-LAST-UPDATE    PIC S9(04)  COMP.                         
           10 WB-NI-KTY-ANSCHREIB-TYP PIC S9(04)  COMP.                         
           10 WB-NI-KNR-ANSCHREIB-TYP PIC S9(04)  COMP.                         
           10 WB-NI-BA-DRUCK-DATUM    PIC S9(04)  COMP.                         
           10 WB-NI-BA-PROGRAMM-ID    PIC S9(04)  COMP.                         
           10 WB-NI-BA-USER-ID        PIC S9(04)  COMP.                         
        05  WB-VIVB0073.                                                        
           10 WB-MA-ST-NR             PIC S9(9) USAGE COMP  VALUE ZERO.         
           10 WB-BA-DAT-VON           PIC X(26)             VALUE SPACE.        
           10 WB-BA-LAST-UPDATE       PIC X(26)             VALUE SPACE.        
           10 WB-KTY-ANSCHREIB-TYP    PIC S9(4) USAGE COMP  VALUE ZERO.         
           10 WB-KNR-ANSCHREIB-TYP    PIC S9(4) USAGE COMP  VALUE ZERO.         
           10 WB-BA-DRUCK-DATUM       PIC X(26)             VALUE SPACE.        
           10 WB-BA-PROGRAMM-ID       PIC X(08)             VALUE SPACE.        
           10 WB-BA-USER-ID           PIC X(08)             VALUE SPACE.        
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *        
      ******************************************************************        
