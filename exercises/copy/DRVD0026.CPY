      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0026.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0026.CPv  $
      *
      *   Rev 2.1   30 Jan 2006 17:18:30   T13AE23
      *AVD00026: Vergl. Umrechnung über VBI -Eingereichtes
      *Geschäf
      *t -
      *
      *   Rev 2.0   Jan 16 2006 15:45:22   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *  COPY-MEMBER DATEN ROSA STATISTIK EINGEREICHTES GESCHÄFT     *
      *              DEUTSCHLAND BZW. ÖSTERREICH FÜR VBI             *
      *              PROTOKOLLDATEI                                  *
      ****************************************************************
      * LOGBUCH                                                      *
      * AUFTRAG ! DATUM  ! AENDERUNG                 ! NAME          *
      * --------+--------+---------------------------+---------------*
      * AVD00026!16.01.06! NEU                       ! RAHNER        *
      *         !        !                           !               *
      *         !        !                           !               *
      ****************************************************************
       01 AFILE-PRKL.
           05 P-ZEILE-SST-NAME                 PIC X(08).
           05 P-ZEILE-ZEITSTEMPEL-BEGINN.
              10 P-ZEILE-Z-STEMPEL-BEG-JJJJ    PIC 9(04).
              10 P-ZEILE-Z-STEMPEL-BEG-STRICH1 PIC X(01).
              10 P-ZEILE-Z-STEMPEL-BEG-MM      PIC 9(02).
              10 P-ZEILE-Z-STEMPEL-BEG-STRICH2 PIC X(01).
              10 P-ZEILE-Z-STEMPEL-BEG-TT      PIC 9(02).
              10 P-ZEILE-Z-STEMPEL-BEG-STRICH3 PIC X(01).
              10 P-ZEILE-Z-STEMPEL-BEG-HH      PIC 9(02).
              10 P-ZEILE-Z-STEMPEL-BEG-PUNKT1  PIC X(01).
              10 P-ZEILE-Z-STEMPEL-BEG-MI      PIC 9(02).
              10 P-ZEILE-Z-STEMPEL-BEG-PUNKT2  PIC X(01).
              10 P-ZEILE-Z-STEMPEL-BEG-SS      PIC 9(02).
              10 P-ZEILE-Z-STEMPEL-BEG-PUNKT3  PIC X(01).
              10 P-ZEILE-Z-STEMPEL-BEG-MILIS   PIC 9(06).
           05 P-ZEILE-ZEITSTEMPEL-ENDE.
              10 P-ZEILE-Z-STEMPEL-END-JJJJ    PIC 9(04).
              10 P-ZEILE-Z-STEMPEL-END-STRICH1 PIC X(01).
              10 P-ZEILE-Z-STEMPEL-END-MM      PIC 9(02).
              10 P-ZEILE-Z-STEMPEL-END-STRICH2 PIC X(01).
              10 P-ZEILE-Z-STEMPEL-END-TT      PIC 9(02).
              10 P-ZEILE-Z-STEMPEL-END-STRICH3 PIC X(01).
              10 P-ZEILE-Z-STEMPEL-END-HH      PIC 9(02).
              10 P-ZEILE-Z-STEMPEL-END-PUNKT1  PIC X(01).
              10 P-ZEILE-Z-STEMPEL-END-MI      PIC 9(02).
              10 P-ZEILE-Z-STEMPEL-END-PUNKT2  PIC X(01).
              10 P-ZEILE-Z-STEMPEL-END-SS      PIC 9(02).
              10 P-ZEILE-Z-STEMPEL-END-PUNKT3  PIC X(01).
              10 P-ZEILE-Z-STEMPEL-END-MILIS   PIC 9(06).
           05 P-ZEILE-ANZAHL-SAETZE            PIC X(09).
           05 P-ZEILE-JOBNAME                  PIC X(08).
