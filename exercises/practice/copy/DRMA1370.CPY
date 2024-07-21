      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1370.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1370.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:53:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     mitCopy: Tabelle OVBEZEICH00
      *
      *     SOURCE-NAME      :  DRMA1370
      *     DMR-NAME         :  DG-OV1370
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  21.05.08/14:33
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 BEZEICH.
              07 BEZEICH-GRP-ID         PIC S9(4)      BINARY.
              07 BEZEICH-LFDNR          PIC S9(4)      BINARY.
              07 BEZEICH-KURZ           PIC X(4).
              07 BEZEICH-MITTEL         PIC X(10).
              07 BEZEICH-LANG           PIC X(40).
              07 AENDERUNGSSYSZEIT      PIC X(26).
              07 AENDERUNGSKENNUNG      PIC X(8).
      *
      **************************************************************
