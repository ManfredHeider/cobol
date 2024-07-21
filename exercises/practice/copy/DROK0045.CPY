      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0045.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0045.CPv  $
      *
      *   Rev 2.1   28 Oct 2008 15:49:24   t13ex27
      *AKI00743: DTA-Kontrolle Teil 2
      *
      *   Rev 2.0   Oct 19 2006 15:03:12   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0045                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0045                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00473!14.08.08!2.1 ! DTA-Kontrolle für OKI     !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0045
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0045.
           05  PI-DROK0045-KEY.

             10 PI-DROK0045-AK-LOGMAS-GEN        PIC X(8).

      *
       01  PO-DROK0045.
           05  PO-DROK0045-RC                    PIC  X(003).
               88 PO-DROK0045-RC-OK                        VALUE 'OK '.
               88 PO-DROK0045-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DROK0045-DATEN.

             10 PO-DROK0045-AK-KEY               PIC X(26).

      *
      ******* ENDE COPY-MEMBER **************************************
