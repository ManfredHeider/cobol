      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0119.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0119.CPv  $
      *
      *   Rev 2.1   May 29 2012 10:21:20   t13ex27
      *AOK00047: Auswertung Einarbeitungszeiten und Satzstatistik-D
      *B und OZPs
      *
      *   Rev 2.0   Mar 08 2012 15:46:16   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0119                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0119                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         09.03.2012 / 18:11:33 / T13EX27
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
      *    FÜR DIE FUNKTION DROK0119
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0119.
           05  PI-DROK0119-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DROK0119-OKEJAHR   PIC S9(4) COMP.
             10 PI-DROK0119-OKEMONAT   PIC S9(4) COMP.
END   *


           05  PI-DROK0119-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DROK0119-OKE-AKKEY-MAX   PIC  X(026).
             10 PI-DROK0119-OKE-DAUER-GES   PIC S9(9) COMP.
             10 PI-DROK0119-OKE-DAUER-10000   PIC S9(4) COMP.
             10 PI-DROK0119-OKE-SATZ-ANZ-GES   PIC S9(9) COMP.
             10 PI-DROK0119-OKE-SATZ-MAX   PIC S9(9) COMP.
             10 PI-DROK0119-OKE-SQL-ANZ-GES   PIC S9(9) COMP.
             10 PI-DROK0119-OKE-SQL-MAX   PIC S9(9) COMP.
             10 PI-DROK0119-OKEAKKEY-LOG-EIN   PIC  X(026).
END   *

      *
       01  PO-DROK0119.
           05  PO-DROK0119-RC                     PIC  X(003).
               88 PO-DROK0119-RC-OK                  VALUE 'OK '.
               88 PO-DROK0119-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
