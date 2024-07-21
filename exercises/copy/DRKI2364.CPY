      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.10.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2364.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2364.CPv  $
      *
      *   Rev 2.1   Jan 27 2010 09:47:44   t13ex27
      *AKI00888: Reob-Tabellen aus Abgleich entfernen
      *
      *   Rev 2.0   Oct 08 2009 14:54:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2364                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2364                    *
      *    --------------------------------------------------------   *
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00888!10.08.10! NEU!                           ! IFR      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2364
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
       01  PI-DRKI2364.
           05  PI-DRKI2364-KEY.
             10 PI-DRKI2364-VRBPERSONENID               PIC X(13).
             10 PI-DRKI2364-VRBPERSONENROLLEKZ          PIC X(01).
      *
       01  PO-DRKI2364.
           05  PO-DRKI2364-RC                           PIC  X(003).
               88 PO-DRKI2364-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2364-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2364-DATEN.
             10 PO-DRKI2364-VRBPABADRESSEID              PIC X(13).
             10 PO-DRKI2364-VRBVERTRAGSID                PIC X(13).
      *
      ******* ENDE COPY-MEMBER **************************************
