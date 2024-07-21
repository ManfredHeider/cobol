      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   WVVBD004.cpy  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/WVVBD004.CPv  $                                    
      *
      *   Rev 2.2   Jan 14 2003 11:19:48   t13ae28
      *AVB00649: Dim 7ukunft - Neue Bef.Bed. RGS - D (2.)
      *
      *   Rev 2.1   Jan 14 2003 10:24:46   t13ae28
      *AVB00649: Dim 7uknft - Neue Bef.Bed. RGS-D
      *                                                                         
      *   Rev 2.0   Nov 26 2002 10:03:04   t13ex15                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: WVVBD004                                      *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: D.Ehrmann                                        *         
      *    DATUM:    26.11.2002                                       *         
      *    STAND:    26.11.2002                                       *         
      *    --------------------------------------------------------   *         
      *    VERSION:  001                                              *         
      *****************************************************************         
      *                                                                         
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      * AVB00649!26.11.02!3.0 ! Neu                       ! D.Ehrmann*          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *                                                                         
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VIVBA009                           *        
      ******************************************************************        
        05  WB-NI-VIVBD004-TAB.                                                 
           10  WB-NI-VIVBD004-ELE  PIC S9(04)  COMP   OCCURS  31 TIMES.         
        05  WB-NI-VIVBD004           REDEFINES   WB-NI-VIVBD004-TAB.            
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.                          
           10 WB-NI-MA-LAST-UPDATE   PIC S9(04)  COMP.                          
           10 WB-NI-KTY-MA-TYP       PIC S9(04)  COMP.                          
           10 WB-NI-KNR-MA-TYP       PIC S9(04)  COMP.                          
           10 WB-NI-MA-BDV-MITGLIED  PIC S9(04)  COMP.                          
           10 WB-NI-MA-STRAFE        PIC S9(04)  COMP.                          
           10 WB-NI-MA-VERSANDSTOP   PIC S9(04)  COMP.                          
           10 WB-NI-MA-ZV-ZVII-ABL   PIC S9(04)  COMP.                          
           10 WB-NI-MA-RCK-STZ       PIC S9(04)  COMP.                          
           10 WB-NI-MA-RCK-STZ-DAT   PIC S9(04)  COMP.                          
           10 WB-NI-MA-FUEH-ZEUG-DAT PIC S9(04)  COMP.                          
           10 WB-NI-MA-SCHUFA-DAT    PIC S9(04)  COMP.                          
           10 WB-NI-MA-34C-DAT       PIC S9(04)  COMP.                          
           10 WB-NI-MA-AFO           PIC S9(04)  COMP.                          
           10 WB-NI-MA-DARLEHEN      PIC S9(04)  COMP.                          
           10 WB-NI-MA-AVAG-DVAG     PIC S9(04)  COMP.                          
           10 WB-NI-MA-KOMPASS-AVAG  PIC S9(04)  COMP.                          
           10 WB-NI-MA-AUTOTELEFON   PIC S9(04)  COMP.                          
           10 WB-NI-MA-TELETEXT      PIC S9(04)  COMP.                          
           10 WB-NI-MA-ZV-NR         PIC S9(04)  COMP.                          
           10 WB-NI-MA-ZV-DAT-BIS-R  PIC S9(04)  COMP.                          
           10 WB-NI-MA-ZV-DAT-BIS    PIC S9(04)  COMP.                          
           10 WB-NI-MA-ZV-DAT-VON    PIC S9(04)  COMP.                          
           10 WB-NI-MA-ZV-KUE-DAT    PIC S9(04)  COMP.                          
           10 WB-NI-MA-ED-ZUSATZ     PIC S9(04)  COMP.                          
           10 WB-NI-MA-ZV-VERL       PIC S9(04)  COMP.                          
           10 WB-NI-MA-FG-DAT        PIC S9(04)  COMP.                          
           10 WB-NI-MA-127-DAT       PIC S9(04)  COMP.                          
           10 WB-NI-MA-VA-DAT        PIC S9(04)  COMP.                          
           10 WB-NI-MA-ANWERBER      PIC S9(04)  COMP.                          
           10 WB-NI-MA-BEFBED-NEU    PIC S9(04)  COMP.                          
        05  WB-VIVBD004.                                                        
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.         
           10 WB-MA-LAST-UPDATE    PIC X(26)                VALUE SPACE.        
           10 WB-KTY-MA-TYP        PIC S9(4)   COMP         VALUE ZERO.         
           10 WB-KNR-MA-TYP        PIC S9(4)   COMP         VALUE ZERO.         
           10 WB-MA-BDV-MITGLIED   PIC X(1)                 VALUE SPACE.        
           10 WB-MA-STRAFE         PIC X(1)                 VALUE SPACE.        
           10 WB-MA-VERSANDSTOP    PIC X(1)                 VALUE SPACE.        
           10 WB-MA-ZV-ZVII-ABL    PIC X(1)                 VALUE SPACE.        
           10 WB-MA-RCK-STZ        PIC S999V99   COMP-3     VALUE ZERO.         
           10 WB-MA-RCK-STZ-DAT    PIC X(10)                VALUE SPACE.        
           10 WB-MA-FUEH-ZEUG-DAT  PIC X(10)                VALUE SPACE.        
           10 WB-MA-SCHUFA-DAT     PIC X(10)                VALUE SPACE.        
           10 WB-MA-34C-DAT        PIC X(10)                VALUE SPACE.        
           10 WB-MA-AFO            PIC X(1)                 VALUE SPACE.        
           10 WB-MA-DARLEHEN       PIC X(1)                 VALUE SPACE.        
           10 WB-MA-AVAG-DVAG      PIC X(10)                VALUE SPACE.        
           10 WB-MA-KOMPASS-AVAG   PIC X(10)                VALUE SPACE.        
           10 WB-MA-AUTOTELEFON    PIC X(14)                VALUE SPACE.        
           10 WB-MA-TELETEXT       PIC X(14)                VALUE SPACE.        
           10 WB-MA-ZV-NR          PIC X(1)                 VALUE SPACE.        
           10 WB-MA-ZV-DAT-BIS-R   PIC X(10)                VALUE SPACE.        
           10 WB-MA-ZV-DAT-BIS     PIC X(10)                VALUE SPACE.        
           10 WB-MA-ZV-DAT-VON     PIC X(10)                VALUE SPACE.        
           10 WB-MA-ZV-KUE-DAT     PIC X(10)                VALUE SPACE.        
           10 WB-MA-ED-ZUSATZ      PIC S9(04)  COMP         VALUE ZERO.         
           10 WB-MA-ZV-VERL        PIC X(1)                 VALUE SPACE.        
           10 WB-MA-FG-DAT         PIC X(10)                VALUE SPACE.        
           10 WB-MA-127-DAT        PIC X(10)                VALUE SPACE.        
           10 WB-MA-VA-DAT         PIC X(10)                VALUE SPACE.        
           10 WB-MA-ANWERBER       PIC S9(9)   COMP         VALUE ZERO.         
           10 WB-MA-BEFBED-NEU     PIC X(10)                VALUE SPACE.        
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 31      *        
      ******************************************************************        
