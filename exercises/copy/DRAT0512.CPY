      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0512.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0512.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:00:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DTA-VORSATZ-NEU.
000020        07 DTA-VORSATZ.
000030           09 SATZ-KZ             PICTURE X.
000040           09 BABS                PICTURE X(27).
000050           09 BERST               PICTURE 9(8).
000060           09 BERST-R REDEFINES BERST.
000070              11 BERST-TT         PICTURE 99.
000080              11 BERST-MM         PICTURE 99.
000090              11 BERST-JJJJ       PICTURE 9(4).
000100           09 BSTAND              PICTURE 9(8).
000110           09 BSTAND-R REDEFINES BSTAND.
000120              11 BSTAND-TT        PICTURE 99.
000130              11 BSTAND-MM        PICTURE 99.
000140              11 BSTAND-JJJJ      PICTURE 9(4).
000150           09 BEMPF               PICTURE X(27).
000160           09 BINHALT             PICTURE X(20).
000170           09 WRGKZN              PICTURE XXX.
000180        07 DTA-VORSATZ-ALLG REDEFINES DTA-VORSATZ.
000190           09 SATZ-KZN            PICTURE X.
000200           09 DTABSENDER          PICTURE X(27).
000210           09 DTERSTELLUNG        PICTURE 9(8).
000220           09 DTSTAND             PICTURE 9(8).
000230           09 DTEMPFAENGER        PICTURE X(27).
000240           09 DTINHALT            PICTURE X(20).
000250           09 DTWAEHRUNGSKZN      PICTURE XXX.
