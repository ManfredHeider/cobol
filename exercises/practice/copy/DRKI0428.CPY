      * T13EX33   Stand: 12/08/98    08:05:20
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0428.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0428.CPv  $
      *
      *   Rev 2.4   Apr 04 2017 14:22:14   t13ac20
      *AKI01503: OZPs bereinigen
      *
      *   Rev 2.3   02 Feb 2009 10:18:06   t13ex27
      *AKI00768: CR 22257 - Bearbeitungskennzeichen Rechenwerte-DB-
      *Änderung
      *
      *   Rev 2.0   Mar 23 1999 15:53:10   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   28 Jul 1998 09:53:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0428                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0428                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.03.2017 / 15:42:41 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI0428
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0428.
           05  PI-DRKI0428-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI0428-LTLLAUFENDENR   PIC S9(4) COMP.
             10 PI-DRKI0428-LTLVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0428-LTLVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI0428-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI0428-N-LTLSZHLBTRG      PIC S9(4) COMP.
             15 PI-DRKI0428-N-LTLVRRNTKPBTR    PIC S9(4) COMP.
END   *
 
           05  PI-DRKI0428-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI0428-LTLAUSZAHLBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0428-LTLAUSZAHLZEITPKT   PIC  X(010).
             10 PI-DRKI0428-LTLAUSZBETRAGBKZ   PIC  X(0001).
             10 PI-DRKI0428-LTLAUSZDECKKAPBETR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0428-LTLRENTEGARZEIT   PIC S9(4) COMP.
             10 PI-DRKI0428-LTLVERRENTKAPBETR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0428-LTLVERRENTKAPBKZ   PIC  X(0001).
             10 PI-DRKI0428-LTLVERRENTKAPBWKZ   PIC  X(0001).
             10 PI-DRKI0428-LTLZAHLZEITPBKZ   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0428-NI1.
           05 PI-DRKI0428-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 2.
       01  PI-DRKI0428-NI7.
           05 PI-DRKI0428-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 2.
 
      *
       01  PO-DRKI0428.
           05  PO-DRKI0428-RC                     PIC  X(003).
               88 PO-DRKI0428-RC-OK                  VALUE 'OK '.
               88 PO-DRKI0428-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

