      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0416.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/CTKI0416.CPv  $
      *
      *   Rev 2.5   Oct 24 2007 16:12:46   t13ex14
      *AKL00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.1   Jul 09 2007 11:42:42   t13ex53
      *AKI00664: Allfinanz: Bestandsdaten (inkl. AM-Vers FK) und Rü
      *ck-DTA Copy und CTKI
      *
      *   Rev 2.0   Jul 09 2007 11:19:58   t13ex53
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00664 !09.07.07!    !-Neu                       !W. Schmidt*
      *         !        !    !                           !          *
      *AKL00664 !24.10.07!    ! MOPVERWFONDS neues Attrib !W. Misgeld*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE MANDANTOPC
      ******************************************************************
CCC005*    EXEC SQL DECLARE MANDANTOPC TABLE
CCC004*    ( MOPAUFTRAG                     CHAR(6) NOT NULL,
CCC004*      MOPMANDANTNR                   INTEGER NOT NULL,
CCC004*      MOPVERWFONDS                   INTEGER  
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  MANDANTOPC
      ******************************************************************
       01  MANDANTOPC-GESAMT.
         05 NI-MANDANTOPC-TAB.
           10 NI-MANDANTOPC-ELE PIC S9(4) COMP OCCURS 3 TIMES.
         05 NI-MANDANTOPC REDEFINES NI-MANDANTOPC-TAB.
           10 NI-MOPAUFTRAG           PIC S9(4) COMP.
           10 NI-MOPMANDANTNR         PIC S9(4) COMP.
           10 NI-MOPVERWFONDS         PIC S9(4) COMP.
         05 DCL-MANDANTOPC.
           10 MOPAUFTRAG           PIC X(6).
           10 MOPMANDANTNR         PIC S9(9) COMP.
           10 MOPVERWFONDS         PIC S9(9) COMP.
      ******************************************************************
