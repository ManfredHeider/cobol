      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1420.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1420.CPv  $
      *
      *   Rev 2.6   16 Jun 2006 12:05:54   t13ae46
      *FPA00855: Korrektur Belastungsdauer bei dit Aufbauplan
      *
      *   Rev 2.5   Jul 21 2005 09:51:36   t13ae46
      *APA00351: dit Aufbauplan
      *
      *   Rev 2.4   Jul 18 2005 12:52:54   t13ae46
      *APA00351: dit Aufbauplan
      *
      *   Rev 2.3   16 Feb 2001 12:09:48   T13AE29
      *APA00126: DIT-VL-Kennzeichen eingefügt
      *
      *   Rev 2.2   19 Dec 2000 14:59:50   T13EX43
      *APA00111 : Wiederholung NTPUT (identisch mit Vorversion)
      *
      *   Rev 2.1   07 Sep 2000 17:54:14   T13AE18
      *APA00112
      *
      *   Rev 2.0   12 Jul 2000 12:32:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1420  PEG-DATENSATZ DER DIT GES. 40       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    07.09.00                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00112!07.09.00!2.1 ! NEU                       !BOHLINGER *
      * APA00126!09.02.01!2.1 ! VL-Kennzeichen eingefügt  !A.Brenner *
      * APA00351!08.07.05!2.4 ! LAUFZEIT, HAFTUNGSDAUER   !WANG      *
      *         !        !    ! BELASTUNGSDAUER HINZU     !          *
      *         !21.07.05!2.5 ! ÄNDERUNGEN                !WANG      *
      * FPA00855!12.06.06!2.6 !HAFTZEITDAUER UND FAELLIG- !WANG      *
      *         !        !    !KEITSDAUER VIERSTELLIG     !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    DIT-DTAS.                                                 *
      *                                                              *
      ****************************************************************
      *
             30 DIT-DATENSATZ.
                35 VNR                              PIC  9(015).
                35 VERTRAG-BEGINN                   PIC  X(010).
                35 FAELLIG-VON                      PIC  X(010).
                35 AVERM                            PIC  9(007).
                35 PGEB-GES                         PIC  9(002).
                35 KUNDE-NNAME                      PIC  X(030).
                35 KUNDE-VNAME                      PIC  X(020).
                35 ABR-DAT-PG                       PIC  X(010).
                35 PIN                              PIC  9(007).
                35 ZAHLWEISE                        PIC  9(002).
                35 VERTRAG-WAEHRUNG                 PIC  X(003).
                35 PGV-PG                           PIC  X(004).
                35 WPKN                             PIC  X(012).
                35 PGV-KENNZEICHEN                  PIC  X(001).
                35 PROV-SATZ                        PIC  9(003)V999.
                35 PROV-BETRAG                      PIC S9(007)V99.
                35 PPFL-SUMME                       PIC S9(007)V99.
                35 SPARBETRAG                       PIC S9(007)V99.
                35 VL-KENNZEICHEN                   PIC  X(002).
210705          35 FILLER                           PIC  X(001).
080705          35 LAUFZEIT                         PIC  X(003).
120606*         35 FILLER                           PIC  X(001).
120606*         35 HAFTUNGSDAUER                    PIC  X(003).
120606          35 HAFTUNGSDAUER                    PIC  X(004).
120606*         35 FILLER                           PIC  X(001).
120606*         35 BELASTUNGSDAUER                  PIC  X(003).
120606          35 BELASTUNGSDAUER                  PIC  X(004).
210705          35 FILLER                           PIC  X(001).
210705*         35 FILLER                           PIC  X(004).
080705*         35 FILLER                           PIC  X(013).
      *
      ******* ENDE COPY-MEMBER **************************************
