      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3085.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA3085.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:11:16   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Adressdaten aus PMMA3086 für BAV-Contolling-Tool
      *
      *     SOURCE-NAME      :  DRMA3085
      *     DMR-NAME         :  DG-OV3085
      *
      *     AUTOR            :  BOOST, MANFRED
      *     GENERIERUNG      :  02.11.05/13:03
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DG-OV3085.
              07 OE                     PICTURE 999.
              07 ORGSAGT                PICTURE 9(7).
              07 NAME                   PICTURE X(40).
              07 VORNAME                PICTURE X(20).
              07 STRASSE                PICTURE X(40).
              07 PLZ                    PICTURE 9(5).
              07 ORT                    PICTURE X(40).
              07 SERVICETELEFON-1       PICTURE X(14).
              07 EMAIL                  PICTURE X(83).
      *
      **************************************************************
