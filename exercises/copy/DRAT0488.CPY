      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.10.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0488.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0488.CPv  $
      *
      *   Rev 2.2   Dec 13 2018 11:34:32   t13ex78
      *AKI01651: G81 - AdvoCard - neue Tarife 2019 (Anwartschaft)
      *
      *   Rev 2.1   Nov 11 2011 14:21:10   t13ex14
      *AKI01079: DB-Objekte: TBAT0039 / VERTRAGSARTBEZ
      *
      *   Rev 2.0   Oct 07 2011 08:08:28   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0488
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01079!07.10.11! 01 ! Neu                       ! Misgeld  *
      * -------------------------------------------------------------*
      * AKI01651!04.12.18!    ! BEZEICHNUNG_KURZ ebenfalls! tfl      *
      *         !        !    ! zurückgeben               !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  PI-DRAT0488.
           05  PI-DRAT0488-KEY.
             10 PI-DRAT0488-KTY               PIC S9(4) COMP.
             10 PI-DRAT0488-KNR               PIC S9(4) COMP.
             10 PI-DRAT0488-BEZEICHNUNG       PIC X(100).
      *
       01  PO-DRAT0488.
           05  PO-DRAT0488-RC                 PIC  X(003).
               88 PO-DRAT0488-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0488-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRAT0488-DATEN.
             10 PO-DRAT0488-BEZEICHNUNG       PIC X(100).
             10 PO-DRAT0488-BEZEICHNUNG-KURZ  PIC X(020).
 
      ******************************************************************
      *          E n d e    Copy-Member DRAT0488                       *
      ******************************************************************

