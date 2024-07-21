      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1257.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1257.CPv  $
      *
      *   Rev 2.3   Apr 04 2017 14:22:50   t13ac20
      *AKI01503: OZPs bereinigen
      *
      *   Rev 2.2   02 Feb 2009 10:18:24   t13ex27
      *AKI00768: CR 22257 - Bearbeitungskennzeichen Rechenwerte-DB-
      *Änderung
      *
      *   Rev 2.1   May 03 2006 13:11:30   t13ex39
      *AKI00215: Umstellung aud EURO  LEBEN GESAMT
      *
      *   Rev 2.0   05 Jul 2001 17:06:54   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1257                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1257                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.03.2017 / 15:43:08 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI1257
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1257.
           05  PI-DRKI1257-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI1257-LTLLAUFENDENR   PIC S9(4) COMP.
             10 PI-DRKI1257-LTLVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1257-LTLVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI1257-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI1257-N-LTLSZHLBTRG      PIC S9(4) COMP.
             15 PI-DRKI1257-N-LTLVRRNTKPBTR    PIC S9(4) COMP.
END   *
 
           05  PI-DRKI1257-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI1257-LTLAUSZAHLBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1257-LTLAUSZAHLZEITPKT   PIC  X(010).
             10 PI-DRKI1257-LTLVERRENTKAPBETR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1257-LTLVERRENTKAPBWKZ   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI1257-NI1.
           05 PI-DRKI1257-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 2.
       01  PI-DRKI1257-NI7.
           05 PI-DRKI1257-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 2.
      *
       01  PO-DRKI1257.
           05  PO-DRKI1257-RC                     PIC  X(003).
               88 PO-DRKI1257-RC-OK               VALUE 'OK '.
               88 PO-DRKI1257-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

