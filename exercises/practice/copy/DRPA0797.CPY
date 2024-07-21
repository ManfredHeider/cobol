      ****************************************************************
      *    COPY-MEMBER: DRPA0797  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    14.01.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!14.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA01539!26.07.13!2.2 ! ERWEITERUNGEN F. KALYPSO  ! RAHNER   *
      * APA02220!11.03.16!2.3 ! PROV-SATZ ERWEITERT       ! BUCH     *
      * APA02353!20.02.17!2.6 ! Teleson                   ! BUCH     *
      * APA02579!26.04.18!2.7 ! FirWi neue DB2-Attribute  ! Hofmann  *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0797                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PMPR TBPA0115              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0797.
          05 PI-DRPA0797-PROGRAMM              PIC  X(008).
          05 PI-DRPA0797-UFKT                  PIC  9(002).
          05 PI-DRPA0797-PMPR-DATEN.
           10 PI-DRPA0797-PMPR-ID             PIC S9(15)V  USAGE COMP-3.
           10 PI-DRPA0797-PMPR-VERSION        PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMBE-ID             PIC S9(15)V  USAGE COMP-3.
           10 PI-DRPA0797-PMBE-VERSION        PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PRINTE              PIC S9(15)V  USAGE COMP-3.
           10 PI-DRPA0797-PGEB-GES            PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PROD-PROD-SCHL      PIC S9(4) BINARY.
           10 PI-DRPA0797-PMPR-PGV            PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-PPFL-BEITRAG PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-PPFL-SUMME   PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-PG-BEITRAG   PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-PG-SUMME     PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-PG-PROV-STZ  PIC S9(4)V9(4) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-PG-GSMT-PROV PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PRKO-KOND-NR-GSP  PIC S9(4)      USAGE COMP.
           10 PI-DRPA0797-PRKO-KOND-NR-AP-FP  PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-PG-DATEN       PIC X(30).
           10 PI-DRPA0797-PMPR-VSTAT          PIC X(4).
           10 PI-DRPA0797-PMPR-VSTAT-UNR      PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-PG-PRODART     PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-KNR-DYN-KZ-1        PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-KNR-DYN-KZ-2        PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-KNR-BEW-HRKFT       PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-BEW-SCHL       PIC S9(9)    USAGE COMP.
           10 PI-DRPA0797-PMPR-PRO-RATA-Z     PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-PRO-RATA-N     PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-BWERT-SUMME  PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-RWERT-SUMME  PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-BZ-DAUER       PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-BEITRAG-JAHR PIC S9(7)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-NEUST-KZ       PIC X(1).
           10 PI-DRPA0797-PMPR-TARIF-SCHLGRP  PIC X(15).
           10 PI-DRPA0797-KNR-TARIF-KZ        PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-TARIF-GRP      PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-DAUER-GSMT     PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-HAFTZ-VON      PIC X(10).
           10 PI-DRPA0797-PMPR-HAFTZ-BIS      PIC X(10).
           10 PI-DRPA0797-PMPR-PG-TARIF       PIC X(15).
           10 PI-DRPA0797-PMPR-RWERT-BEITRAG
                                            PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-PROV-BEITRAG
                                            PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-PROV-VERDIENT PIC S9(4)     USAGE COMP.
           10 PI-DRPA0797-PMPR-VERS-SUMME   PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-AE-KZ         PIC X(1).
           10 PI-DRPA0797-PMPR-TLSN-TARIF    PIC X(50).
           10 PI-DRPA0797-PMPR-TLSN-LAUFZEIT PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-TLSN-VRBRAUCH PIC S9(09)   USAGE COMP.
           10 PI-DRPA0797-KNR-TLSN-STORNO    PIC S9(4)    USAGE COMP.
           10 PI-DRPA0797-PMPR-TLSN-Z-NR     PIC  X(20).
           10 PI-DRPA0797-PMPR-GEL-GEVO      PIC X(7).
           10 PI-DRPA0797-PMPR-GEL-RABST     PIC X(5).
           10 PI-DRPA0797-PMPR-GEL-EALTER    PIC S9(4) USAGE COMP.
           10 PI-DRPA0797-PMPR-NBB-SUMME    PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-AUW-SUMME    PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0797-PMPR-VERTRAGSTEIL  PIC S9(004) BINARY.    
      *
       01 PO-DRPA0797.
          05 PO-DRPA0797-RC                   PIC  X(003).
             88 PO-DRPA0797-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0797-RC-DUPLICATE-VALUE            VALUE 'DVA'.
 
 
 


