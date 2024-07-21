      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.12.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0010.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRRW0010.CPv  $
      *
      *   Rev 2.2   Jan 29 2013 10:49:12   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.1   Jan 28 2013 17:46:54   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.0   Dec 10 2012 09:31:44   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0010                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRRW0010                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRETSCH                                          *
      *    DATUM:    11.12.2012                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01244!11.12.12! 01 ! NEU                       !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRRW0010: I B A N - K U R Z P R Ü F U N G
      *
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRRW0010.
           05  PI-DRRW0010-DATEN.
               10 PI-DRRW0010-IBAN             PIC  X(034).
      *
       01  PO-DRRW0010.
           05  PO-DRRW0010-RC                  PIC  X(003).
               88 PO-DRRW0010-RC-OK                        VALUE 'OK '.
      *--- IBAN-Land nicht in KTY/KNR (BEZ10) vorhanden
               88 PO-DRRW0010-RC-LAND-NFD                  VALUE 'LNF'.
      *--- IBAN-Länge nicht wie in KTY/KNR (BEZ10) definiert
               88 PO-DRRW0010-RC-LAENGE-NOK                VALUE 'LNK'.
           05  PO-DRRW0010-DATEN.
               10 PO-DRRW0010-IBAN-LAND        PIC  X(002).
               10 PO-DRRW0010-IBAN-LAENGE      PIC  9(002).
      *
      ******************** ENDE COPY-MEMBER ***************************

