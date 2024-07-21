      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2549.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2549.CPv  $
      *
      *   Rev 2.1   Feb 03 2012 15:16:26   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.0   Dec 5 2011 15:12:34 T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2549                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2549                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         05.12.2011 / 15:12:34 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI2549
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2549.
           05  PI-DRKI2549-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2549-UBRJAHR   PIC S9(4) COMP.
             10 PI-DRKI2549-UBRMONAT   PIC S9(4) COMP.
             10 PI-DRKI2549-UBRRGID   PIC S9(4) COMP.
             10 PI-DRKI2549-UBRVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2549-UBRVETLFDNR   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI2549.
           05  PO-DRKI2549-RC                     PIC  X(003).
               88 PO-DRKI2549-RC-OK               VALUE 'OK '.
               88 PO-DRKI2549-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2549-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2549-UBRERSTDATUM   PIC  X(010).
             10 PO-DRKI2549-UBRGARRUEWERT   PIC S9(11)V9(4) COMP-3.
             10 PO-DRKI2549-UBRGARTODLEIST   PIC S9(11)V9(4) COMP-3.
             10 PO-DRKI2549-UBRLFDMON   PIC S9(4) COMP.
             10 PO-DRKI2549-UBRMONNR   PIC S9(4) COMP.
             10 PO-DRKI2549-UBRRECHZINS   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI2549-UBRRUEWERTUEB   PIC S9(11)V9(4) COMP-3.
             10 PO-DRKI2549-UBRSTATUS   PIC  X(0001).
             10 PO-DRKI2549-UBRSTILLERES   PIC S9(11)V9(4) COMP-3.
             10 PO-DRKI2549-UBRTODLEIUEB   PIC S9(11)V9(4) COMP-3.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
