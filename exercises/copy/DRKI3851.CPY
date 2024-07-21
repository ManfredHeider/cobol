      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.12.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3851.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3851.CPv  $
      *
      *   Rev 2.1   Jan 06 2021 10:17:04   u130685
      *AKI01821: DVAG Wien Sparplan
      *
      *   Rev 2.0   Dec 22 2020 08:48:37 U130685
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3851                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3851                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.12.2020 / 10:14:45 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01821!22.12.20!1.0 !NEU FUER DVAG WIEN SPARPLAN! U130685  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3851
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3851.
           05  PI-DRKI3851-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3851-G33LFDNR   PIC S9(4) COMP.
             10 PI-DRKI3851-G33VNR   PIC  X(0015).
END   *

           05  PI-DRKI3851-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3851-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3851-G33ABRKTO   PIC  X(0020).
             10 PI-DRKI3851-G33ABWEICHKTOINH   PIC  X(0030).
             10 PI-DRKI3851-G33AUSGABEAKTFOND   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI3851-G33AUSGABEURFOND   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI3851-G33BEGINNDYN   PIC  X(0008).
             10 PI-DRKI3851-G33BEGINNRATENAUS   PIC  X(0008).
             10 PI-DRKI3851-G33BIC   PIC  X(0011).
             10 PI-DRKI3851-G33BONI   PIC S9(3)V9(4) COMP-3.
             10 PI-DRKI3851-G33DATVERTRAGKUEND   PIC S9(9) COMP.
             10 PI-DRKI3851-G33ENDERATENAUS   PIC  X(0008).
             10 PI-DRKI3851-G33IBAN   PIC  X(0034).
             10 PI-DRKI3851-G33ISIN   PIC  X(0012).
             10 PI-DRKI3851-G33KOSTENPROZENT   PIC S9(1)V9(2) COMP-3.
             10 PI-DRKI3851-G33KUENDMERKMAL   PIC  X(0001).
             10 PI-DRKI3851-G33MANDAT   PIC  X(0035).
             10 PI-DRKI3851-G33PIN   PIC S9(9) COMP.
             10 PI-DRKI3851-G33PROZENTDYNAMIS   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI3851-G33SPARPLANANF   PIC  X(0008).
             10 PI-DRKI3851-G33SPARPLANENDE   PIC  X(0008).
             10 PI-DRKI3851-G33SPARPLANENDE2   PIC  X(0008).
             10 PI-DRKI3851-G33SPARRATE   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3851-G33TAGEINZAHLUNG   PIC S9(4) COMP.
             10 PI-DRKI3851-G33URGESAMTEURO   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI3851-G33URSPARRATE   PIC S9(4)V9(2) COMP-3.
             10 PI-DRKI3851-G33VERTRAGLFZ   PIC S9(9) COMP.
             10 PI-DRKI3851-G33VERTRAGSTYP   PIC  X(0002).
             10 PI-DRKI3851-G33WKN   PIC  X(0006).
             10 PI-DRKI3851-G33ZAHLWEISE   PIC S9(9) COMP.
             10 PI-DRKI3851-G33ZLASTRUECK   PIC S9(9) COMP.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3851-NI1.
           05 PI-DRKI3851-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3851-NI7.
           05 PI-DRKI3851-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3851.
           05  PO-DRKI3851-RC                     PIC  X(003).
               88 PO-DRKI3851-RC-OK               VALUE 'OK '.
               88 PO-DRKI3851-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
