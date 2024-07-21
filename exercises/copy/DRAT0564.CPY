      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0564.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0564.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:08:54   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabesatz Agenturpostversand für Control-D
      *
      *     SOURCE-NAME      :  DRAT0564
      *     DMR-NAME         :  DR-KM6615
      *
      *     AUTOR            :  HOLLAX, THOMAS
      *     GENERIERUNG      :  17.05.05/10:04
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRAT0564.
              07 AGENTUR                PICTURE X(8).
              07 POST-AGENTUR           PICTURE X(8).
              07 POST-ANSCHR1           PICTURE X(40).
              07 POST-ANSCHR2           PICTURE X(40).
              07 POST-ANSCHR3           PICTURE X(40).
              07 POST-ANSCHR4           PICTURE X(40).
              07 POST-ANSCHR5           PICTURE X(40).
              07 POST-ANSCHR6           PICTURE X(40).
              07 POST-ANSCHR7           PICTURE X(40).
              07 ORG-EINH-KURZ          PICTURE X(10).
              07 POSTVERS-RHYTHMUS      PICTURE XX.
              07 ZUSTELL-ANSCHR1        PICTURE X(40).
              07 ZUSTELL-ANSCHR2        PICTURE X(40).
              07 ZUSTELL-ANSCHR3        PICTURE X(40).
              07 ZUSTELL-ANSCHR4        PICTURE X(40).
              07 ZUSTELL-ANSCHR5        PICTURE X(40).
              07 ZUSTELL-ANSCHR6        PICTURE X(40).
              07 ZUSTELL-ANSCHR7        PICTURE X(40).
              07 AD-GES-TITEL-KURZ      PICTURE X(10).
              07 AGENTURPOST-WEG        PICTURE XX.
              07 FILLER                 OCCURS 100 TIMES
                                        PICTURE X.
      *
      **************************************************************
