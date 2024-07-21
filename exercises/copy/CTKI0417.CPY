      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0417.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/CTKI0417.CPv  $
      *
      *   Rev 2.10   Nov 15 2007 15:28:44   t13ex14
      *AKL00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.3   Jul 12 2007 10:00:36   t13ex53
      *AKI00664: Allfinanz: Bestandsdaten (inkl. AM-Vers FK) und Rü
      *ck-DTA Copy und CTKI
      *
      *   Rev 2.2   Jul 12 2007 09:51:30   t13ex53
      *AKI00664: Allfinanz: Bestandsdaten (inkl. AM-Vers FK) und Rü
      *ck-DTA Copy und CTKI
      *
      *   Rev 2.1   Jul 09 2007 11:42:52   t13ex53
      *AKI00664: Allfinanz: Bestandsdaten (inkl. AM-Vers FK) und Rü
      *ck-DTA Copy und CTKI
      *
      *   Rev 2.0   Jul 09 2007 11:20:12   t13ex53
      *Archiverstellung
      *
      *---------------------------------------------------------------*
     ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00664 !09.07.07!    !-Neu                       !W. Schmidt*
      *AKI00664 !12.07.07!    !-Neues Attribut            !W. Schmidt*
      *AKI00664 !18.08.07!    !Mandantennr. für INTEGER   !W. Misgeld*
      *AKI00664 !18.09.07!    !VMHVERARBSTATUS entfernt   !W. Misgeld*
      *         !15.11.07!    !VMHSTATUS eingefügt        !I. Frenzel*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE VERTRAGHISTORIE
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERTRAGHISTORIE TABLE
CCC004*    ( VMHPARTGESELLSCHID             SMALLINT NOT NULL,
CCC004*      VMHVERTRAGSNUMMER              CHAR(15) NOT NULL,
CCC004*      VMHLFDNR                       SMALLINT NOT NULL,
CCC004*      VMHVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VMHMANDANTNRALT                INTEGER  NOT NULL,
CCC004*      VMHMANDANTNRNEU                INTEGER  NOT NULL,
CCC004*      VMHJOBNAME                     CHAR(8) NOT NULL,
CCC004*      VMHABSCHLVBNRALT               INTEGER,
CCC004*      VMHABSCHLVBNRNEU               INTEGER,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,   
CCC004*      VMHSTATUS                      CHAR(1) NOT NULL      
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VERTRAGHISTORIE
      ******************************************************************
       01  VERTRAGHISTORIE-GESAMT.
         05 NI-VERTRAGHISTORIE-TAB.
           10 NI-VERTRAGHISTORIE-ELE PIC S9(4) COMP OCCURS 11 TIMES.
         05 NI-VERTRAGHISTORIE REDEFINES NI-VERTRAGHISTORIE-TAB.
           10 NI-VMHPARTGESELLSCHID    PIC S9(4) COMP.
           10 NI-VMHVERTRAGSNUMMER     PIC S9(4) COMP.
           10 NI-VMHLFDNR              PIC S9(4) COMP.
           10 NI-VMHVERTRAGSID         PIC S9(4) COMP.
           10 NI-VMHMANDANTNRALT       PIC S9(4) COMP.
           10 NI-VMHMANDANTNRNEU       PIC S9(4) COMP.
           10 NI-VMHJOBNAME            PIC S9(4) COMP.
           10 NI-VMHABSCHLVBNRALT      PIC S9(4) COMP.
           10 NI-VMHABSCHLVBNRNEU      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG       PIC S9(4) COMP.
           10 NI-VMHSTATUS             PIC S9(4) COMP.
         05 DCL-VERTRAGHISTORIE.
           10 VMHPARTGESELLSCHID   PIC S9(4) COMP.
           10 VMHVERTRAGSNUMMER    PIC X(15).
           10 VMHLFDNR             PIC S9(4) COMP.
           10 VMHVERTRAGSID        PIC X(13).
           10 VMHMANDANTNRALT      PIC S9(9) COMP.
           10 VMHMANDANTNRNEU      PIC S9(9) COMP.
           10 VMHJOBNAME           PIC X(8).
           10 VMHABSCHLVBNRALT     PIC S9(9) COMP.
           10 VMHABSCHLVBNRNEU     PIC S9(9) COMP.
           10 LETZTEAENDERUNG      PIC X(26).
           10 VMHSTATUS            PIC X(1).    
      ******************************************************************
