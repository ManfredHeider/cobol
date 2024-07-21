      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0087.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0087.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 30 1996 14:33:40   T13EX11
      *AFVB0226  Stichtage angepaßt
      *
      *   Rev 1.0   Mar 27 1996 08:39:42   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0087                                      *
      *    --------------------------------------------------------   *
      *    ÜBERGABEBEREICH VOM VB91 (FINANZSTATUS) AN UNTERDIALOGE    *
      *    (ACHTUNG: BEGINNT AUF STUFE 08)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÖLLHAMMER                                     *
      *    DATUM:    02.02.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0226!02.02.96! 01 ! NEU                       ! SCHÖLLH. *
      * AFVB0226!26.04.96! 02 ! STICHTAGE ANGEPASST       ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      ****************************************************************
      * 01 ---> KEINE 01-ER-STUFE, DAMIT DIESE STRUKTUR
      *    --->  AUCH IN CA-SICHERUNG ÜBERNOMMEN WERDEN KANN

           08  DRVB0087.
               10  DRVB0087-KEY-AUFB-ESTNR         PIC  X(007).
               10  DRVB0087-KEY-AUFB-ELETMON       PIC  X(003).
               10  DRVB0087-KEY-AUFB-EMONVON       PIC  X(004).
               10  DRVB0087-KEY-AUFB-EMONBIS       PIC  X(004).

               10  DRVB0087-VB-NR                  PIC  9(007).
               10  DRVB0087-MONVON-ISO             PIC  X(010).
               10  DRVB0087-MONBIS-ISO             PIC  X(010).

260496**           * DRVB0087-STICH-ISO: STICHTAG LT. TABLE KTO
260496*         10  DRVB0087-STICH-ISO              PIC  X(010).
260496**           * DRVB0087-STICH-ANZ-ISO: STICHTAG LT. TABLE KTO
260496**           *  ABER: STATT 28. WIRD DER MONATSLETZTE GESETZT
260496*         10  DRVB0087-STICH-ANZ-ISO          PIC  X(010).

260496*           * AKTUELLSTER EINTRAG IN KTO ZUM BIS-MONAT
260496         10  DRVB0087-STICH-KTO-STAND-ISO    PIC  X(010).
260496*           * ERMITTELTER STICHTAG, ZU DEM KONTO-STÄNDE
260496*           * ANGEZEIGT WERDEN KONNTEN
260496*           * - BEI HISTOR. BIS-MONAT: DER 15. DES MONATS
260496*           * - BEI AKT. BIS-MONAT: TABESDATUM - 1 TAG
260496         10  DRVB0087-STICHTAG-ISO           PIC  X(010).

      *           * KNR NUR FÜR WENIGE UNTERMASKEN INTERESSANT
      *           * (Z. B. FÜR SONSTIGE AUSZAHLUNGEN NACH MONATEN)
               10  DRVB0087-KNR                    PIC  9(004).

      * (ACHTUNG: MAX 128 BYTES, DA ÜBER CADD-I-KOMM-VAR ÜBERTRAGEN !!)
      *
      ******** ENDE COPY-MEMBER (DRVB0087) *******************

