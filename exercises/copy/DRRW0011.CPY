      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.12.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0011.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRRW0011.CPv  $
      *
      *   Rev 2.5   Jun 19 2013 11:39:48   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN - Einarbeitu
      *ng IBAN-Regelwerk in IBAN-Konverter 1.0 UPRW0001
      *
      *   Rev 2.4   Feb 22 2013 13:08:46   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.3   Feb 12 2013 18:52:24   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.2   Jan 29 2013 10:49:18   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.1   Jan 28 2013 17:47:02   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.0   Dec 10 2012 09:31:54   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0011                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRRW0011                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRETSCH                                          *
      *    DATUM:    11.02.2013                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01244!11.02.13! 01 ! SEPA-Anpassung            !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRRW0011: B I C - V A L I D I E R U N G
      *
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRRW0011.
           05  PI-DRRW0011-DATEN.
               10 PI-DRRW0011-BIC              PIC  X(011).
               10 PI-DRRW0011-IBAN             PIC  X(034).
      *
       01  PO-DRRW0011.
           05  PO-DRRW0011-RC                  PIC  X(003).
               88 PO-DRRW0011-RC-OK                        VALUE 'OK '.
      *--- BIC in BLZ-Tabelle nicht gefunden
               88 PO-DRRW0011-RC-BIC-NFD                   VALUE 'NFD'.
      *--- BIC passt nicht zur IBAN
               88 PO-DRRW0011-RC-BICIBAN-NOK               VALUE 'PNK'.
      *--- Land aus IBAN nicht DE oder AT
               88 PO-DRRW0011-RC-LZ-NO-DE-AT               VALUE 'NDA'.
      *--- IBAN-Land nicht in KTY/KNR (BEZ10) vorhanden
               88 PO-DRRW0011-RC-LAND-NFD                  VALUE 'LNF'.
      *--- IBAN-Länge nicht wie in KTY/KNR (BEZ10) definiert
               88 PO-DRRW0011-RC-LAENGE-NOK                VALUE 'LNK'.
           05  PO-DRRW0011-DATEN.
               10 PO-DRRW0011-LZ               PIC  X(002).
               10 PO-DRRW0011-BLZ              PIC  X(011).
               10 PO-DRRW0011-BANKNAME         PIC  X(058).
      *
      ******************** ENDE COPY-MEMBER ***************************
