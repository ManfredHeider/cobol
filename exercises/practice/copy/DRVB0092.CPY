      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0092.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0092.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Mar 27 1996 08:41:12   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************  00001039
      *    COPY-MEMBER: DRVB0092, SCHNITTSTELLE UPVB0077             *  00002039
      *                 ED-PROVISION NACH ED-KONTEN  XA08            *  00002141
      *    --------------------------------------------------------  *  00003039
      *    ERSTELLT: AUERBACH                                        *  00004039
      *    DATUM:    27.02.1996                                      *  00005039
      ****************************************************************  00006039
      *    VERSION
      ****************************************************************  00008039
      * AENDERUNGEN:                                                 *  00009039
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00009139
      * -------------------------------------------------------------*  00009239
      * AFVB0226!27.02.96!    ! NEU                       ! AUERBACH *  00009339
      *         !        !    !                           !          *  00009839
      *         !        !    !                           !          *  00009839
      *                                                              *  00009939
      * ENDE-LOGBUCH                                                 *  00010039
      ****************************************************************  00010139
      *                                                                 00042600
      *   PI-UPVB0077-FKT                                               00052106
      *      PI-VB77-FKT-ED-PROV-KONTEN                                 00052206
      *         ERMITTELN UND SCHREIBEN ED-PROVISION NACH ED-KONTEN
      *         IN TS-QUEUE ZU ÜBERGEBENER VB-NR UND ZEITRAUM
      *                                                                 00042600
      *   PI-UPVB0077-VB-NR
      *         VB, DER IN DEM ÜBERGEBENEN ZEITRAUM EVTL. EINEN         00052206
      *         EINTRAG IN DER DATENBANK HAT (MUSS ABER NICHT SEIN !!!) 00048104
      *                                                                 00048104
      *   PI-UPVB0077-MONVON-ISO
      *   PI-UPVB0077-MONBIS-ISO
      *         ANFANGS- UND ENDEMONAT DES ZEITRAUMS IM ISO-FORMAT.     00048104
      *         TAG VON MONVON IST IMMER = '01'                         00048104
      *         TAG VON MONBIS IST IMMER = (LETZTER TAG DES MONATS)     00048104
      *                                                                 00048104
      *   PI-UPVB0077-TSQ-NAME                                          00051754
      *         NAME DER TS-QUEUE, IN DIE DIE SÄTZE ZU SCHREIBEN SIND   00048104
      *                                                                 00048104
      *   PI-UPVB0077-ZEILE-DATEN-MAX                                   00051754
      *         MAX ANZAHL ZEILEN, INNERHALB(!!!) DERER EINE ZWISCHEN-  00048104
      *         SUMME ERMITTELT UND IN DIE TS-QUEUE GESCHRIEBEN WERDEN  00048104
      *         MUSS.                                                   00048104
      *         BEISPIEL: WERT=13; (SPÄTESTENS) DIE 13. ZEILE MUSS      00048104
      *         EINE ZWISCHENSUMME ENTHALTEN                            00048104
      *                                                                 00048104
      ***************************************************************   00051036
      *                 PARAMETER-INPUT                             *   00051136
      ***************************************************************   00051236
       01  PI-UPVB0077.                                                 00051605
           05  PI-UPVB0077-FKT                PIC  X(003).              00052106
               88 PI-VB77-FKT-ED-PROV-KONTEN            VALUE 'EPK'.    00052206

           05  PI-UPVB0077-VB-NR              PIC  9(007).
           05  PI-UPVB0077-MONVON-ISO         PIC  X(010).
           05  PI-UPVB0077-MONBIS-ISO         PIC  X(010).
           05  PI-UPVB0077-TSQ-NAME           PIC  X(008).              00051754
           05  PI-UPVB0077-ZEILE-DATEN-MAX    PIC S9(004) BINARY.       00051754
      *                                                                 00052500
      ***************************************************************   00052636
      *                 PARAMETER-OUTPUT                            *   00052736
      ***************************************************************   00052836
       01  PO-UPVB0077.                                                 00052905

      *    * ERGEBNIS:                                                  00063536
           05  PO-UPVB0077-RC                 PIC  9(001).                  0006
               88  PO-VB77-RC-OK                           VALUE 1.         0006
               88  PO-VB77-RC-KEINE-DATEN                  VALUE 2.         0006
               88  PO-VB77-RC-FKT-UNGUELT                  VALUE 3.         0006
               88  PO-VB77-RC-TSQNAME-FEHLT                VALUE 4.         0006

      *    * ANZAHL GESCHRIEBENE ITEMS                                  00063536
           05  PO-UPVB0077-ITEM-ANZ           PIC S9(004) BINARY.              0
                                                                        00063336
      *    * ENDEZEILE MIT GESAMTSUMME(N) BEREITS DRUCKAUFBEREITET      00063536
           05  PO-UPVB0077-ENDEZEILE          PIC  X(079).                     0
                                                                        00063336
      *    * SCHALTER WENN DATEN NUR TEILWEISE ANGEZEIGT WERDEN KÖNNEN  00063536
           05  PO-VB77-DATEN                  PIC  X(001).                  0006
               88  PO-VB77-DATEN-INI                       VALUE ' '.       0006
               88  PO-VB77-DATEN-TEILWEISE                 VALUE 'T'.       0006
                                                                        00063336
      ******** ENDE COPY-MEMBER   DRVB0092   ***********************    00452014
                                                                        00470001
