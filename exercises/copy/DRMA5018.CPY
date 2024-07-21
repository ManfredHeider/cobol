      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5018.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA5018.CPv  $
      *
      *   Rev 2.4   Sep 05 2013 13:53:46   t13ac20
      *AMA00011: Kalypso: Teilprojekt 4: allgemeine Funktionen
      *
      *   Rev 2.3   Aug 19 2013 12:13:48   t13ac20
      *AMA00011: Kalypso: Teilprojekt 4: allgemeine Funktionen
      *
      *   Rev 2.2   Jul 16 2013 08:38:36   t13ac20
      *AMA00011: Kalypso: Teilprojekt 4: allgemeine Funktionen
      *
      *   Rev 2.1   Jul 08 2013 10:56:58   t13ac20
      *AMA00011: Kalypso: Teilprojekt 4: allgemeine Funktionen
      *
      *   Rev 2.0   Jul 04 2013 15:01:14   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5004   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5018
      *
      *     GENERIERUNG      :  05.09.2013/13:11:39
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!04.07.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00020!04.07.13!2.2  ! NEUERSTELLUNG             ! WOLFF
      * AMA00020!04.07.13!2.3  ! NEUERSTELLUNG             ! WOLFF
      * AMA00020!05.09.13!2.4  ! LOGBUCH AKTUALISIERT      ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5018.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 AUSWAHL-EIN-DATUM      pic XX.
              07 AUSWAHL-AUS-DATUM      pic XX.
              07 DATUM                  pic X(30).
              07 DATUM-EUR redefines DATUM
                                        pic X(10).
              07 DATUM-JJJJMMTT1 redefines DATUM
                                        pic 9(8).
              07 DATUM-JJJJMMTT2 redefines DATUM
                                        pic S9(9)      packed-decimal.
              07 DATUM-JJJJMMTTHHMMSSS redefines DATUM
                                        pic X(15).
      *
      **************************************************************
