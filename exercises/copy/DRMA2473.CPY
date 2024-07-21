      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2473.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2473.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:32:46   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1253 - INSERT
      *
      *     SOURCE-NAME      :  DRMA2473
      *     DMR-NAME         :  DG-OV2473
      *
      *     AUTOR            :  WORGULL, THOMAS
      *     GENERIERUNG      :  17.10.01/11:15
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DG-OV2473.
000020        07 FUNKTION               PICTURE X(8).
000030        07 BUCHDAT.
000040           09 VERSION             PICTURE X.
000050           09 VG                  PICTURE S9(4)  BINARY.
000060           09 VERARB-KZN          PICTURE X.
000070           09 FACH-KENNUNG        PICTURE X(10).
000080           09 BUCH-DAT            PICTURE X(10).
000090           09 DV-KENNUNG          PICTURE X(10).
000100           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000110           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************
