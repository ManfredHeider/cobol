      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0129.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0129.CPv  $
      *
      *   Rev 2.1   Oct 08 2012 14:02:16   t13ex27
      *AOK00047: Auswertung Einarbeitungszeiten und Satzstatistik-S
      *atzstatistik
      *
      *   Rev 2.0   Sep 11 2012 17:43:32 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0129                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0129                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.09.2012 / 17:43:32 / T13EX27
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
      *    FÜR DIE FUNKTION DROK0129
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0129.
           05  PI-DROK0129-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DROK0129-OTSDATUM   PIC  X(010).
             10 PI-DROK0129-OTSTABELLE   PIC  X(0030).
END   *


           05  PI-DROK0129-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DROK0129-OTSDELETE   PIC S9(9) COMP.
             10 PI-DROK0129-OTSINSERT   PIC S9(9) COMP.
             10 PI-DROK0129-OTSUPDATE   PIC S9(9) COMP.
END   *

      *
       01  PO-DROK0129.
           05  PO-DROK0129-RC                     PIC  X(003).
               88 PO-DROK0129-RC-OK                  VALUE 'OK '.
               88 PO-DROK0129-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
