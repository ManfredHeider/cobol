      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2652.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2652.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:35:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1253 - SELECT-3
      *
      *     SOURCE-NAME      :  DRMA2652
      *     DMR-NAME         :  DG-OV2652
      *
      *     AUTOR            :  NIEßEN, SVEN
      *     GENERIERUNG      :  14.03.03/08:42
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA2652.
              07 FUNKTION               PICTURE X(8).
              07 VERSION                PICTURE X.
              07 VG                     PICTURE S9(4)  BINARY.
              07 FACH-KENNUNG           PICTURE X(10).
              07 BUCH-DAT               PICTURE X(10).
      *
      **************************************************************
