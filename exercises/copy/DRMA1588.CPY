      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1588.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1588.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:11:48   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1581 - DELETE
      *
      *     SOURCE-NAME      :  DRMA1588
      *     DMR-NAME         :  DG-OV1588
      *
      *     AUTOR            :  KOCHS, ANDREAS
      *     GENERIERUNG      :  20.07.00/14:42
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1588.
000020        07 FUNKTION               PICTURE X(8).
000030        07 BEZEICH-GRP-ID         PICTURE S9(4)  BINARY.
000040        07 BEZEICH-LFDNR          PICTURE S9(4)  BINARY.
000050        07 AENDERUNGSSYSZEIT      PICTURE X(26).
      *
      **************************************************************
