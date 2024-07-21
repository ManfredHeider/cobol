      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0295.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA0295.CPv  $
      *
      *   Rev 2.7   Jun 09 2020 11:25:38   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.6   Apr 04 2018 15:35:12   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.5   Feb 28 2017 09:22:50   T13ex24
      *APA02275: DAISY-Erweiterungen zu FlexN - PEG  Module
      *
      *   Rev 2.4   Feb 10 2017 15:49:36   t13ex24
      *APA02275: DAISY-Erweiterungen zu FlexN - PEG  Module
      *
      *   Rev 2.3   23 Aug 2007 09:52:28   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.2   Feb 22 2000 12:56:48   T13EX22
      *OPPA0064: 2.DTA-Satz-Erweiterung
      *
      *   Rev 2.1   Jan 28 2000 10:25:46   T13EX22
      *OPPA0064: Erweiterungen für Währungsprüfung
      *
      *   Rev 2.0   Feb 27 1999 14:28:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:47:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:32:00   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0295  PEG-DATENSATZ DES AMV-DTAS          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    16.05.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!16.05.97!    ! NEU                       ! WOLFF    *
      * APA00082!26.01.00! 1.1! DATENSATZERWEITERUNG      ! MUND     *
      * APA00082!16.02.00!    ! DTA-SATZ ERWEITERUNG      ! MUND     *
      * APA00639!21.06.07!2.3 ! ERW. DER PIN AUF 9 STELLEN! WANG     *
      * APA02275!30.01.17!2.4 ! ERWEITERT UM FlexN-Felder ! KÖPRÜCÜ  *
      * APA02579!22.03.18!2.6 ! ERWEITERT TARIF           ! WANG     *
      * APA02908!17.02.20!2.7 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    AMV-DTAS.                                                 *
      *                                                              *
      ****************************************************************
      *
             30 AMV-DATENSATZ.
                35 VNR                              PIC  9(015).
170220          35 VVGES1                           PIC  9(002).
                35 VVNR1                            PIC  9(015).
170220          35 VVGES2                           PIC  9(002).
                35 VVNR2                            PIC  9(015).
170220          35 VVGES3                           PIC  9(002).
                35 VVNR3                            PIC  9(015).
170220          35 VVGES4                           PIC  9(002).
170220          35 VVNR4                            PIC  9(015).
170220          35 VVGES5                           PIC  9(002).
170220          35 VVNR5                            PIC  9(015).
170220          35 VVGES6                           PIC  9(002).
170220          35 VVNR6                            PIC  9(015).
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
                35 SYSTEM-KENNZEICHEN               PIC  X(004).
                35 GV-TIMESTAMP                     PIC  X(026).
                35 VSP-KOMBI-NR                     PIC  9(005).
                35 VSP-KOMBI-BZ                     PIC  X(003).
                35 VSP-FN-BER                       PIC  X(001).
                35 VSP-FN-REL                       PIC  X(001).
170220          35 VERTR-KENNZ                      PIC  9(004).
                35 TAB-INDEX                        PIC  9(002).
                35 PRODUKT-TABELLE.
                   40 PRODUKT-ZEILE                 OCCURS 45
                                                INDEXED BY IX-AMV-PROD.
                      45 PGV-PG                     PIC  9(004).
                      45 TARIF                      PIC  X(013).
                      45 PPFL-BEITRAG               PIC S9(007)V99.
                      45 PROV-SATZ                  PIC  9(003)V99.
                      45 PROV-BETRAG                PIC S9(007)V99.
                      45 JAHRES-NT-PRAEMIE          PIC S9(007)V99.
                35 FILLER                           PIC  X(030).
      *
      ******* ENDE COPY-MEMBER **************************************
 



