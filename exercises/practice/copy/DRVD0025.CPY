      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0025.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0025.CPv  $
      *
      *   Rev 2.1   30 Jan 2006 17:18:22   T13AE23
      *AVD00026: Vergl. Umrechnung über VBI -Eingereichtes
      *Geschäf
      *t -
      *
      *   Rev 2.0   Jan 16 2006 15:45:10   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *  COPY-MEMBER DATEN ROSA STATISTIK EINGEREICHTES GESCHÄFT     *
      *              DEUTSCHLAND BZW. ÖSTERREICH FÜR VBI             *
      *                                                              *
      ****************************************************************
      * LOGBUCH                                                      *
      * AUFTRAG ! DATUM  ! AENDERUNG                 ! NAME          *
      * --------+--------+---------------------------+---------------*
      * AVD00026!16.01.06! NEU                       ! RAHNER        *
      *         !        !                           !               *
      *         !        !                           !               *
      ****************************************************************
       01  AFILE-VBI-REC.
           10 AUS-VBI-DIR               PIC S9(3) COMP.
           10 AUS-VBI-TABELLE.
              20 AUS-VBI-EINHEITEN      PIC S9(7)V9 COMP-3 OCCURS 24.
           10 AUS-VBI-VBNR              PIC S9(09)  COMP.
           10 AUS-VBI-STATISTIK-DATUM.
              20 AUS-VBI-STATISTIK-DATUM-JHJJ PIC X(4).
              20 AUS-VBI-STATISTIK-DATUM-MM   PIC X(2).

