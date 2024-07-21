      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1564.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA1564.CPv  $
      *
      *   Rev 2.7   Jun 09 2020 11:33:12   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.6   Apr 04 2018 15:35:44   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.5   Feb 28 2017 09:24:10   T13ex24
      *APA02275: DAISY-Erweiterungen zu FlexN - PEG  Module
      *
      *   Rev 2.4   Feb 13 2017 08:55:34   t13ex24
      *APA02275: DAISY-Erweiterungen zu FlexN - PEG  Module
      *
      *   Rev 2.3   31 Aug 2007 14:43:24   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.2   23 Aug 2007 09:54:48   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.1   Nov 04 2003 13:42:00   t13ex21
      *APA00253 - Neuerstellung
      *
      *   Rev 2.0   Jul 04 2002 15:30:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1564  PEG-DATENSATZ DES AMV-DTAS ÜBER     *
      *                           KOMPASS
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    10.10.03                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00238!10.10.03!2.01! NEU                       ! STORCH   *
      * APA00639!21.06.07!2.12! ERW. DER PIN AUF 9 STELLEN! WANG     *
      *         !31.08.07!2.3 ! KORREKTUR                 ! WANG     *
      * APA02275!30.01.17!2.4 ! ERWEITERT UM FlexN-Felder ! KÖPRÜCÜ  *
      * APA02579!23.03.18!2.6 ! TARIF ANPASSUNG           ! WANG     *
      * APA02908!17.02.20!2.7 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    AMV-DTAS ÜBER KOMPASS.                                    *
      *                                                              *
      ****************************************************************
      *
             30 AMV-KOMPASS-DATENSATZ.
                35 VNR                              PIC  9(015).
                35 VVGES1                           PIC  9(002).
                35 VVNR1                            PIC  9(015).
                35 VVGES2                           PIC  9(002).
                35 VVNR2                            PIC  9(015).
                35 VVGES3                           PIC  9(002).
                35 VVNR3                            PIC  9(015).
                35 VVGES4                           PIC  9(002).
                35 VVNR4                            PIC  9(015).
                35 VVGES5                           PIC  9(002).
                35 VVNR5                            PIC  9(015).
                35 VVGES6                           PIC  9(002).
                35 VVNR6                            PIC  9(015).
                35 VERTRAG-BEGINN                   PIC  X(010).
                35 VERTRAG-ABLAUF                   PIC  X(010).
                35 FAELLIG-VON                      PIC  X(010).
                35 FAELLIG-BIS                      PIC  X(010).
                35 AVERM                            PIC  9(007).
                35 PGEB-GES                         PIC  9(002).
                35 KUNDE-NAME                       PIC  X(025).
                35 KUNDE-VNAME                      PIC  X(020).
                35 ABR-DAT-PG                       PIC  X(010).
                35 EINL-DAT-PG                      PIC  X(010).
                35 PIN                              PIC  9(009).
                35 ZAHLWEISE                        PIC  9(003).
                35 WAEHRUNG                         PIC  X(003).
                35 SYSTEM-KENNZEICHEN               PIC  X(003).
                35 GV-TIMESTAMP                     PIC  X(026).
                35 VSP-KOMBI-NR                     PIC  9(005).
                35 VSP-KOMBI-BZ                     PIC  X(003).
                35 VSP-FN-BER                       PIC  X(001).
                35 VSP-FN-REL                       PIC  X(001).
                35 VERTR-KENNZ                      PIC  9(004).
                35 TAB-INDEX                        PIC  9(002).
                35 PRODUKT-TABELLE.
                   40 PRODUKT-ZEILE                 OCCURS 45
                                                INDEXED BY IX-AMV-PROD.
                      45 PGV-PG                     PIC  9(004).
                      45 BUCHREG                    PIC  X(005).
                      45 PRODSCHL                   PIC  9(008).
                      45 PPFL-BEITRAG               PIC S9(011)V99.
                      45 PROV-SATZ                  PIC S9(005)V99.
                      45 PROV-BETRAG                PIC S9(011)V99.
                      45 JAHRES-NT-PRAEMIE          PIC S9(011)V99.
                      45 VERTR-T-VERSION            PIC  9(015).
                35 FILLER                           PIC  X(069).
      *
      ******* ENDE COPY-MEMBER **************************************
 



