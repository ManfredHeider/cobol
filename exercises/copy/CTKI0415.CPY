      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0415.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0415.CPv  $
      *
      *   Rev 2.2   30 Aug 2007 09:40:38   t13ex39
      *AKL00664: WERNER HORNUNG
      *
      *   Rev 2.1   Jul 09 2007 11:42:34   t13ex53
      *AKI00664: Allfinanz: Bestandsdaten (inkl. AM-Vers FK) und Rü
      *ck-DTA Copy und CTKI
      *
      *   Rev 2.0   Jul 09 2007 11:19:42   t13ex53
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00664 !09.07.07!    !-Neu                       !W. Schmidt*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE VERTRAGMANDZUST
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERTRAGMANDZUST TABLE
CCC004*    ( VMZPARTGESELLID                SMALLINT NOT NULL,
CCC004*      VMZVERTRAGSNUMMER              CHAR(15) NOT NULL,
CCC004*      VMZMANDANTNRALT                INTEGER  NOT NULL,
CCC004*      VMZMANDANTNRNEU                INTEGER  NOT NULL,
CCC004*      VMZABSCHLVBNRALT               INTEGER,
CCC004*      VMZABSCHLVBNRNEU               INTEGER,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VERTRAGMANDZUST
      ******************************************************************
       01  VERTRAGMANDZUST-GESAMT.
         05 NI-VERTRAGMANDZUST-TAB.
           10 NI-VERTRAGMANDZUST-ELE PIC S9(4) COMP OCCURS 7 TIMES.
         05 NI-VERTRAGMANDZUST REDEFINES NI-VERTRAGMANDZUST-TAB.
           10 NI-VMZPARTGESELLID      PIC S9(4) COMP.
           10 NI-VMZVERTRAGSNUMMER    PIC S9(4) COMP.
           10 NI-VMZMANDANTNRALT      PIC S9(4) COMP.
           10 NI-VMZMANDANTNRNEU      PIC S9(4) COMP.
           10 NI-VMZABSCHLVBNRALT     PIC S9(4) COMP.
           10 NI-VMZABSCHLVBNRNEU     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-VERTRAGMANDZUST.
           10 VMZPARTGESELLID      PIC S9(4) USAGE COMP.
           10 VMZVERTRAGSNUMMER    PIC X(15).
           10 VMZMANDANTNRALT      PIC S9(9) USAGE COMP.
           10 VMZMANDANTNRNEU      PIC S9(9) USAGE COMP.
           10 VMZABSCHLVBNRALT     PIC S9(9) USAGE COMP.
           10 VMZABSCHLVBNRNEU     PIC S9(9) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
