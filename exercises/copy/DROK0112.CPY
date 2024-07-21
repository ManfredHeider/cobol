      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0112.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0112.CPv  $
      *
      *   Rev 2.1   Mar 22 2010 16:00:18   t13ex27
      *AKI00793: Einarbeitungs-/Einarbeitungszeiten in OKI
      *
      *   Rev 2.0   Mar 22 2010 14:47:52   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00793!22.03.10!2.1 ! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  D-ABLAUFKONTROLLE.
               15  FILLER                PIC X(19)
                           VALUE '##ABLAUFKONTROLLE'.
               15  FILLER                PIC X(01) VALUE SPACE.
      * Kennzeichnung 'ANFANG' oder 'ENDE'
               15  D-AK-ANF-END          PIC X(06).
               15  FILLER                PIC X(01) VALUE SPACE.
               15  D-AK-MANDANT          PIC 9(07).
               15  FILLER                PIC X(01) VALUE SPACE.
               15  D-AK-GESELLSCHAFT     PIC 9(02).
               15  FILLER                PIC X(01) VALUE SPACE.
               15  D-AK-JOBNAME          PIC X(08).
               15  FILLER                PIC X(01) VALUE SPACE.
               15  D-AK-LFDNR            PIC 9(10).
               15  FILLER                PIC X(01) VALUE SPACE.
      * ABKBEGAKTU oder ABKENDAKTU
               15  D-AK-TIMESTAMP        PIC X(26).
               15  FILLER                PIC X(01) VALUE SPACE.
               15  FILLER                PIC X(02) VALUE '##'.
