      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1330.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/MF/CPY/DRPA1330.CPv  $
      *
      *   Rev 2.7   Nov 29 2001 10:51:00   t13ae46
      *APA00156: Erweiterung um 2 Stellen des Verkaufsschlüssels  
      *
      * 
      *
      *   Rev 2.6   23 May 2000 09:57:38   T13AE18
      * 
      *
      *   Rev 2.5   Apr 04 2000 08:35:32   T13AE18
      *apa00067
      *
      *   Rev 2.4   Mar 30 2000 09:04:26   T13AE18
      *APA00067
      *
      *   Rev 2.3   Mar 20 2000 13:43:28   T13AE18
      *APA00067
      *
      *   Rev 2.2   Feb 29 2000 10:54:42   T13AE18
      *APA00067
      *
      *   Rev 2.0   Dec 14 1999 16:42:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1330  PEG-DATENSATZ DER BADENIA GES. 70   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    31.01.00                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00067!31.01.00!2.0 ! NEU                       ! BOHLINGER*
      *         !23.05.00!2.6 ! PROV-SATZ AUF 3 NK ERW.   ! BOHLINGER*
      * APA00156!14.11.01!2.7 ! ERWEITERUNG UM 2 STELLEN  ! WANG     *
      *         !        !    ! DES VERKAUFSSCHLÜSSELS    !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    AMV-DTAS.                                                 *
      *                                                              *
      ****************************************************************
      *
             30 BADENIA-DATENSATZ.
                35 VNR                              PIC  9(011).
                35 VERTRAG-BEGINN                   PIC  9(008).
                35 AVERM                            PIC  9(007).
                35 PGEB-GES                         PIC  9(002).
                35 KUNDE-NNAME                      PIC  X(025).
                35 KUNDE-VNAME                      PIC  X(020).
                35 ABR-DAT-PG                       PIC  9(008).
                35 DAT-LETZTE-KD-ZAHL               PIC  9(008).
                35 PIN                              PIC  9(007).
                35 VERTRAG-WAEHRUNG                 PIC  X(003).
                35 PGV-PG                           PIC  9(004).
141101*         35 TARIF                            PIC  9(003).
141101          35 TARIF                            PIC  9(005).
                35 VERTRAG-SUMME                    PIC S9(009)V99.
                35 PPFL-SUMME                       PIC S9(009)V99.
230500          35 PROV-SATZ                        PIC  9(003)V999.
                35 PROV-BETRAG                      PIC S9(007)V99.
                35 VERTRAG-SPLIT-KZ                 PIC  X(001).
      *
      ******* ENDE COPY-MEMBER **************************************
