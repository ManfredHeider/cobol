      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.02.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0028.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0028.CPv  $
      *
      *   Rev 2.1   Mar 12 2020 16:35:30   t13ae56
      *ASL00584: PGM - Upro für CTV-Briefe
      *
      *   Rev 2.0   Feb 25 2020 14:05:52   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VBL-51  !25.02.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *            COPY-MEMBER FÜR CTV-BRIEFE VHV-STEUERBESCHEINIGUNG *
      *            DATEINUMMER: CV01140_ VSAM                         *
      *            COPYMEMBER:  DRCV0028                              *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRCV0028-SATZ.
           02 DRCV0028-VB-NR                    PIC 9(007).
           02 DRCV0028-VS-ANZAHL                PIC 9(001).
           02 DRCV0028-VS-NR-1                  PIC 9(008).
           02 DRCV0028-VS-BETR-1                PIC ZZZ9,99.
           02 DRCV0028-VS-NR-2                  PIC 9(008).
           02 DRCV0028-VS-BETR-2                PIC ZZZ9,99.
           02 DRCV0028-VS-NR-3                  PIC 9(008).
           02 DRCV0028-VS-BETR-3                PIC ZZZ9,99.
           02 DRCV0028-VS-NR-4                  PIC 9(008).
           02 DRCV0028-VS-BETR-4                PIC ZZZ9,99.
           02 DRCV0028-HALB-JAHR                PIC 9(001).
           02 DRCV0028-JAHR                     PIC 9(004).
      *
      **** ENDE DRCV0028
      *
