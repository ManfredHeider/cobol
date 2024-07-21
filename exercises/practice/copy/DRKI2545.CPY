      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2545.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2545.CPv  $
      *
      *   Rev 2.1   Feb 03 2012 15:16:04   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.0   Dec 5 2011 15:12:21 T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2545                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2545                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         05.12.2011 / 15:12:21 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI2545
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2545.
           05  PI-DRKI2545-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2545-UBRJAHR   PIC S9(4) COMP.
             10 PI-DRKI2545-UBRMONAT   PIC S9(4) COMP.
             10 PI-DRKI2545-UBRRGID   PIC S9(4) COMP.
             10 PI-DRKI2545-UBRVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2545-UBRVETLFDNR   PIC S9(4) COMP.
END   *


           05  PI-DRKI2545-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2545-UBRERSTDATUM   PIC  X(010).
             10 PI-DRKI2545-UBRGARRUEWERT   PIC S9(11)V9(4) COMP-3.
             10 PI-DRKI2545-UBRGARTODLEIST   PIC S9(11)V9(4) COMP-3.
             10 PI-DRKI2545-UBRLFDMON   PIC S9(4) COMP.
             10 PI-DRKI2545-UBRMONNR   PIC S9(4) COMP.
             10 PI-DRKI2545-UBRRECHZINS   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2545-UBRRUEWERTUEB   PIC S9(11)V9(4) COMP-3.
             10 PI-DRKI2545-UBRSTATUS   PIC  X(0001).
             10 PI-DRKI2545-UBRSTILLERES   PIC S9(11)V9(4) COMP-3.
             10 PI-DRKI2545-UBRTODLEIUEB   PIC S9(11)V9(4) COMP-3.
END   *

      *
       01  PO-DRKI2545.
           05  PO-DRKI2545-RC                     PIC  X(003).
               88 PO-DRKI2545-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2545-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
