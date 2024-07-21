      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0130.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0130.CPv  $
      *
      *   Rev 2.1   Oct 08 2012 14:02:22   t13ex27
      *AOK00047: Auswertung Einarbeitungszeiten und Satzstatistik-S
      *atzstatistik
      *
      *   Rev 2.0   Sep 11 2012 17:43:35 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0130                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0130                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.09.2012 / 17:43:35 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0130
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0130.
           05  PI-DROK0130-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DROK0130-OTSDATUM   PIC  X(010).
             10 PI-DROK0130-OTSTABELLE   PIC  X(0030).
END   *


           05  PI-DROK0130-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DROK0130-OTSDELETE   PIC S9(9) COMP.
             10 PI-DROK0130-OTSINSERT   PIC S9(9) COMP.
             10 PI-DROK0130-OTSUPDATE   PIC S9(9) COMP.
END   *


      *
       01  PO-DROK0130.
           05  PO-DROK0130-RC                     PIC  X(003).
               88 PO-DROK0130-RC-OK               VALUE 'OK '.
               88 PO-DROK0130-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
