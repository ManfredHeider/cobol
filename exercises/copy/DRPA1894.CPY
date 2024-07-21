      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.07.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1894.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1894.CPv  $
      *
      *   Rev 2.2   Jun 09 2020 11:34:24   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.1   Dec 10 2018 13:41:14   t13ae37
      *APA02749: Einführung GC&C - Ersatz für Ventilproduktschlüsse
      *l 0091/0093 - Sourcen ABU
      *
      *   Rev 2.0   Jul 24 2018 13:52:52   T13EX44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1894  PEG-DATENSATZ DES GC&C-DTAS         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    16.05.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02749!26.11.18!2.1 ! NEU                       ! BUCH     *
      * APA02908!17.02.20!2.2 ! GES-UEBERGR. EVS          ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    AMV-DTAS.                                                 *
      *                                                              *
      ****************************************************************
              30 GCC-DATENSATZ.
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
                                                INDEXED BY IX-GCC-PROD.
                      45 PGV-PG                     PIC  9(004).
                      45 TARIF                      PIC  X(013).
                      45 PPFL-BEITRAG               PIC S9(007)V99.
                      45 PROV-SATZ                  PIC  9(003)V99.
                      45 PROV-BETRAG                PIC S9(007)V99.
                      45 JAHRES-NT-PRAEMIE          PIC S9(007)V99.
      *
      ******* ENDE COPY-MEMBER **************************************
 


