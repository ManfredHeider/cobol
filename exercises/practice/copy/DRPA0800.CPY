      ****************************************************************
      *    COPY-MEMBER: DRPA0800  Funktion Select-all Standard-OZP   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    14.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!14.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA00452!01.11.06! 2.1! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      * APA01539!26.07.13! 2.2! ERWEITERUNG F. KALYPSO    ! RAHNER   *
      * APA02220!11.03.16! 2.3! PROV-SATZ ERWEITERT       ! BUCH     *
      * APA02353!20.02.17! 2.5! Teleson                   ! BUCH     *
      * APA02579!26.04.18! 2.6! FrWI neue DB2-Attribute   ! Hofmann  *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0800                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>SELECT ALL<<<       TABELLE PMPR TBPA0115              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0800.
          05 PI-DRPA0800-PMPR-KEY.
           10 PI-DRPA0800-PMPR-ID             PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0800-PMPR-VERSION        PIC S9(4) USAGE COMP.
      *
       01 PO-DRPA0800.
          05 PO-DRPA0800-RC                   PIC  X(003).
             88 PO-DRPA0800-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0800-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0800-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0800-PMPR-DATEN.
           10 PO-DRPA0800-PMPR-LAST-UPDATE    PIC X(26).
           10 PO-DRPA0800-PMPR-VERSION        PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-KENNUNG        PIC X(08).
           10 PO-DRPA0800-PMPR-V-GUELTIG-AB   PIC X(26).
           10 PO-DRPA0800-PMPR-V-GUELTIG-BIS  PIC X(10).
           10 PO-DRPA0800-PMPR-V-KENNUNG      PIC X(08).
           10 PO-DRPA0800-PMBE-ID             PIC S9(15)V USAGE COMP-3.
           10 PO-DRPA0800-PMBE-VERSION        PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PRINTE              PIC S9(15)V USAGE COMP-3.
           10 PO-DRPA0800-PGEB-GES            PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PROD-PROD-SCHL      PIC S9(4) BINARY.
           10 PO-DRPA0800-PMPR-PGV            PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-PPFL-BEITRAG PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-PPFL-SUMME   PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-PG-BEITRAG   PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-PG-SUMME     PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-PG-PROV-STZ  PIC S9(4)V9(4) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-PG-GSMT-PROV PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PRKO-KOND-NR-GSP  PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PRKO-KOND-NR-AP-FP  PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-PG-DATEN       PIC X(30).
           10 PO-DRPA0800-PMPR-VSTAT          PIC X(4).
           10 PO-DRPA0800-PMPR-VSTAT-UNR      PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-PG-PRODART     PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-KNR-DYN-KZ-1        PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-KNR-DYN-KZ-2        PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-KNR-BEW-HRKFT       PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-BEW-SCHL       PIC S9(9) USAGE COMP.
           10 PO-DRPA0800-PMPR-PRO-RATA-Z     PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-PRO-RATA-N     PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-BWERT-SUMME  PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-RWERT-SUMME  PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-BZ-DAUER       PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-BEITRAG-JAHR PIC S9(7)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-NEUST-KZ       PIC X(1).
           10 PO-DRPA0800-PMPR-TARIF-SCHLGRP  PIC X(15).
           10 PO-DRPA0800-KNR-TARIF-KZ        PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-TARIF-GRP      PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-DAUER-GSMT     PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-HAFTZ-VON      PIC X(10).
           10 PO-DRPA0800-PMPR-HAFTZ-BIS      PIC X(10).
           10 PO-DRPA0800-PMPR-PG-TARIF       PIC X(15).
           10 PO-DRPA0800-PMPR-RWERT-BEITRAG
                                            PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-PROV-BEITRAG
                                            PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-PROV-VERDIENT  PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-VERS-SUMME   PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-AE-KZ          PIC X(1).
           10 PO-DRPA0800-PMPR-TLSN-TARIF    PIC X(50).
           10 PO-DRPA0800-PMPR-TLSN-LAUFZEIT PIC S9(4)    USAGE COMP.
           10 PO-DRPA0800-PMPR-TLSN-VRBRAUCH PIC S9(09)   USAGE COMP.
           10 PO-DRPA0800-KNR-TLSN-STORNO    PIC S9(4)    USAGE COMP.
           10 PO-DRPA0800-PMPR-TLSN-Z-NR     PIC X(20).
           10 PO-DRPA0800-PMPR-GEL-GEVO      PIC X(7).
           10 PO-DRPA0800-PMPR-GEL-RABST     PIC X(5).
           10 PO-DRPA0800-PMPR-GEL-EALTER    PIC S9(4) USAGE COMP.
           10 PO-DRPA0800-PMPR-NBB-SUMME    PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-AUW-SUMME    PIC S9(9)V9(2) USAGE COMP-3.
           10 PO-DRPA0800-PMPR-VERTRAGSTEIL  PIC S9(004) BINARY.
      *
      ****** Ende Copy-Member ****************************************
 
 


