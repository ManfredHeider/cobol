      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA006.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA006.CPv  $
      *
      *   Rev 2.1   Jun 26 2006 11:26:58   t13ae13
      *AVB00852: Anpassung VB-System an "Neue Komm.Strategie": SWL 
      *
      *   Rev 2.0   Jun 19 2006 13:08:46   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: WVVBA006                                      * 
      ***************************************************************** 
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AVB00852!20.06.06!2.1 ! NEU                       ! S. Walb  *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA006                           *
      ******************************************************************
        05  WB-NI-VIVBA006-TAB.
           10  WB-NI-VIVBA006-ELE    PIC S9(04)  COMP   OCCURS 13 TIMES.
        05  WB-NI-VIVBA006           REDEFINES   WB-NI-VIVBA006-TAB.
           10 WB-NI-DIR-NR           PIC S9(04)  COMP.
           10 WB-NI-DIR-LAST-UPDATE  PIC S9(04)  COMP.
           10 WB-NI-KTY-DIR-TYP      PIC S9(04)  COMP.
           10 WB-NI-KNR-DIR-TYP      PIC S9(04)  COMP.
           10 WB-NI-KTY-REGION       PIC S9(04)  COMP.
           10 WB-NI-KNR-REGION       PIC S9(04)  COMP.
           10 WB-NI-KTY-RESERV-TYP   PIC S9(04)  COMP.
           10 WB-NI-KNR-RESERV-TYP   PIC S9(04)  COMP.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-DIR-NAME         PIC S9(04)  COMP.
           10 WB-NI-DIR-INFORMATION  PIC S9(04)  COMP.
           10 WB-NI-KTY-VERTRBER     PIC S9(04)  COMP.
           10 WB-NI-KNR-VERTRBER     PIC S9(04)  COMP.
        05  WB-VIVBA006.
           10 WB-DIR-NR            PIC S9(4)   COMP         VALUE ZERO.
           10 WB-DIR-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-KTY-DIR-TYP       PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-DIR-TYP       PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-REGION        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-REGION        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-RESERV-TYP    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-RESERV-TYP    PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-DIR-NAME          PIC X(40)                VALUE SPACE.
           10 WB-DIR-INFORMATION   PIC X(150)               VALUE SPACE.
           10 WB-KTY-VERTRBER      PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-VERTRBER      PIC S9(4)   COMP         VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
