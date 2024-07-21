      * T13EX33   Stand: 19/02/99    09:54:08
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0808.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0808.CPv  $
      *
      *   Rev 2.4   Apr 04 2017 14:22:26   t13ac20
      *AKI01503: OZPs bereinigen
      *
      *   Rev 2.3   02 Feb 2009 10:18:16   t13ex27
      *AKI00768: CR 22257 - Bearbeitungskennzeichen Rechenwerte-DB-
      *Änderung
      *
      *   Rev 2.0   Mar 23 1999 15:53:22   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   12 Jan 1999 15:11:40   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0808                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0808                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.03.2017 / 15:42:59 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI0808
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0808.
           05  PI-DRKI0808-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI0808-LTLLAUFENDENR   PIC S9(4) COMP.
             10 PI-DRKI0808-LTLVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0808-LTLVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI0808-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI0808-N-LTLSZHLBTRG      PIC S9(4) COMP.
             15 PI-DRKI0808-N-LTLVRRNTKPBTR    PIC S9(4) COMP.
END   *
 
           05  PI-DRKI0808-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI0808-LTLAUSZAHLBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0808-LTLAUSZAHLZEITPKT   PIC  X(010).
             10 PI-DRKI0808-LTLAUSZBETRAGBKZ   PIC  X(0001).
             10 PI-DRKI0808-LTLAUSZDECKKAPBETR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0808-LTLRENTEGARZEIT   PIC S9(4) COMP.
             10 PI-DRKI0808-LTLVERRENTKAPBETR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0808-LTLVERRENTKAPBKZ   PIC  X(0001).
             10 PI-DRKI0808-LTLVERRENTKAPBWKZ   PIC  X(0001).
             10 PI-DRKI0808-LTLZAHLZEITPBKZ   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0808-NI1.
           05 PI-DRKI0808-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 2.
       01  PI-DRKI0808-NI7.
           05 PI-DRKI0808-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 2.
      *
       01  PO-DRKI0808.
           05  PO-DRKI0808-RC                     PIC  X(003).
               88 PO-DRKI0808-RC-OK               VALUE 'OK '.
               88 PO-DRKI0808-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

