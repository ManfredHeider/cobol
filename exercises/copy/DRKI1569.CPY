      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1569.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1569.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 12:03:08   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   May 19 2003 10:00:08   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1569                                     *
      *    Ändern ADRESSEID innerhalb der PERSONADRESSBEZ            *
      *    -------------------------------------------------------   *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00114!01.08.03! 01 ! NEUERSTELLUNG             !Peter Pohl*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1569                                 *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1569.
          05 PI-DRKI1569-DATEN.
             10 PI-DRKI1569-PERSONENID       PIC X(013).
             10 PI-DRKI1569-ADRESSEID-ALT    PIC X(013).
             10 PI-DRKI1569-ADRESSEID-NEU    PIC X(013).
             10 PI-DRKI1569-ADRESSARTID      PIC S9(4) COMP.

      *
       01 PO-DRKI1569.
          05 PO-DRKI1569-DATEN.
             10 PO-DRKI1569-Vertragsadresse      PIC  X(001).
      *         J / N
             10 PO-DRKI1569-RC-NOK-Ursache       PIC  X(001).
      *         1 = ADRESSEID-ALT DELETE nicht erfolgreich
      *         2 = ADRESSEID-NEU INSERT nicht erfolgreich
      *         3 = Adresse löschen erfolgt


          05 PO-DRKI1569-RC                      PIC  X(003).
             88 PO-DRKI1569-RC-OK                         VALUE 'OK '.
             88 PO-DRKI1569-RC-NOK                        VALUE 'NOK'.
      *
      ****** Ende Copy-Member ****************************************


