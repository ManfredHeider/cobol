      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1221.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1221.CPv  $
      *
      *   Rev 2.2   May 27 1999 13:11:30   T01JM2E
      *Übergabe nach Produktion APA00042
      *
      *   Rev 2.1   Apr 22 1999 18:41:14   T01JM2E
      * 
      *
      *   Rev 2.0   Mar 30 1999 08:44:06   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:26   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
            05 DTA-VORSATZ-NEU.
               07 DTA-VORSATZ.
                  09 SATZ-KZ             PICTURE X.
                  09 BABS                PICTURE X(27).
                  09 BERST               PICTURE 9(8).
                  09 BERST-R REDEFINES BERST.
                     11 BERST-TT         PICTURE 99.
                     11 BERST-MM         PICTURE 99.
                     11 BERST-JJJJ       PICTURE 9(4).
                  09 BSTAND              PICTURE 9(8).
                  09 BSTAND-R REDEFINES BSTAND.
                     11 BSTAND-TT        PICTURE 99.
                     11 BSTAND-MM        PICTURE 99.
                     11 BSTAND-JJJJ      PICTURE 9(4).
                  09 BEMPF               PICTURE X(27).
                  09 BINHALT             PICTURE X(20).
                  09 WRGKZN              PICTURE XXX.
               07 DTA-VORSATZ-ALLG REDEFINES DTA-VORSATZ.
                  09 SATZ-KZ             PICTURE X.
                  09 DTABSENDER          PICTURE X(27).
                  09 DTERSTELLUNG        PICTURE 9(8).
                  09 DTSTAND             PICTURE 9(8).
                  09 DTEMPFAENGER        PICTURE X(27).
                  09 DTINHALT            PICTURE X(20).
                  09 DTWAEHRUNGSKZN      PICTURE XXX.
