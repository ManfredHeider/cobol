      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1586.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1586.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:11:26   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1581 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1586
      *     DMR-NAME         :  DG-OV1586
      *
      *     AUTOR            :  KOCHS, ANDREAS
      *     GENERIERUNG      :  20.07.00/14:39
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1586.
000020        07 FUNKTION               PICTURE X(8).
000030        07 BEZEICH.
000040           09 BEZEICH-GRP-ID      PICTURE S9(4)  BINARY.
000050           09 BEZEICH-LFDNR       PICTURE S9(4)  BINARY.
000060           09 BEZEICH-KURZ        PICTURE X(4).
000070           09 BEZEICH-MITTEL      PICTURE X(10).
000080           09 BEZEICH-LANG        PICTURE X(40).
000090           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000100           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************
