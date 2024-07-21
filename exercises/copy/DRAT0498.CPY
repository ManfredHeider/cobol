      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.12.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0498.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0498.CPv  $
      *
      *   Rev 2.1   Jan 29 2013 10:37:30   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.0   Dec 11 2012 18:15:40   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0498                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0498                    *
      *    BEANTWORTET DIE FRAGE, OB ZU KTY UND BEZ10                 *
      *    EIN (RC-OK) ODER MEHRERE EINTRÄGE (RC-MULT-ROWS) EXISTIEREN*
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    12.12.2012                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01244!12.12.12! 01 ! NEU                       !B.PRETSCH *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0498: LESEN TBAT0008  KNR
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0498.
           05  PI-DRAT0498-DATEN.
               10 PI-DRAT0498-KTY-NR           PIC S9(004) COMP.
               10 PI-DRAT0498-KNR-BEZ10-LZ     PIC  X(002).
      *
       01  PO-DRAT0498.
           05  PO-DRAT0498-RC                  PIC  X(003).
               88 PO-DRAT0498-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0498-RC-MULT-ROWS                 VALUE 'MUR'.
               88 PO-DRAT0498-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRAT0498-DATEN.
               10 PO-DRAT0498-KNR-NR           PIC S9(004) COMP.
               10 PO-DRAT0498-KNR-BEZ10        PIC  X(010).
      *

