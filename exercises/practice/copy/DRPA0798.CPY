      ****************************************************************
      *    COPY-MEMBER: DRPA0798  Funktion Update Standard-OZP       *
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
      * APA01539!26.07.13!2.1 ! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA02220!11.03.16!2.2 ! PROV-SATZ ERWEITERT       ! BUCH     *
      * APA02579!26.04.18!2.14! FriWi neue DB2-Attribute  ! Hofmann  *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0798                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PMPR TBPA0115              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0798.
          05 PI-DRPA0798-LAST-UPDATE         PIC  X(026).
          05 PI-DRPA0798-PMPR-KEY.
           10 PI-DRPA0798-PMPR-ID             PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0798-PMPR-VERSION        PIC S9(4) USAGE COMP.
          05 PI-DRPA0798-PMPR-DATEN.
           10 PI-DRPA0798-PMBE-ID             PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0798-PMBE-VERSION        PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PRINTE              PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0798-PGEB-GES            PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PROD-PROD-SCHL      PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-PGV            PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-PPFL-BEITRAG PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-PPFL-SUMME   PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-PG-BEITRAG   PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-PG-SUMME     PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-PG-PROV-STZ  PIC S9(4)V9(4) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-PG-GSMT-PROV PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PRKO-KOND-NR-GSP  PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PRKO-KOND-NR-AP-FP  PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-PG-DATEN       PIC X(30).
           10 PI-DRPA0798-PMPR-VSTAT          PIC X(4).
           10 PI-DRPA0798-PMPR-VSTAT-UNR      PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-PG-PRODART     PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-KNR-DYN-KZ-1        PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-KNR-DYN-KZ-2        PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-KNR-BEW-HRKFT       PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-BEW-SCHL       PIC S9(9) USAGE COMP.
           10 PI-DRPA0798-PMPR-PRO-RATA-Z     PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-PRO-RATA-N     PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-BWERT-SUMME  PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-RWERT-SUMME  PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-BZ-DAUER     PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-BEITRAG-JAHR PIC S9(7)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-NEUST-KZ       PIC X(1).
           10 PI-DRPA0798-PMPR-TARIF-SCHLGRP  PIC X(15).
           10 PI-DRPA0798-KNR-TARIF-KZ        PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-TARIF-GRP      PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-DAUER-GSMT     PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-HAFTZ-VON      PIC X(10).
           10 PI-DRPA0798-PMPR-HAFTZ-BIS      PIC X(10).
           10 PI-DRPA0798-PMPR-PG-TARIF       PIC X(15).
           10 PI-DRPA0798-PMPR-RWERT-BEITRAG
                                            PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-PROV-BEITRAG PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-PROV-VERDIENT  PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-VERS-SUMME   PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-AE-KZ          PIC X(1).
           10 PI-DRPA0798-PMPR-TLSN-TARIF    PIC X(50).
           10 PI-DRPA0798-PMPR-TLSN-LAUFZEIT PIC S9(4)    USAGE COMP.
           10 PI-DRPA0798-PMPR-TLSN-VRBRAUCH PIC S9(09)   USAGE COMP.
           10 PI-DRPA0798-KNR-TLSN-STORNO    PIC S9(4)    USAGE COMP.
           10 PI-DRPA0798-PMPR-TLSN-Z-NR     PIC  X(20).
           10 PI-DRPA0798-PMPR-GEL-GEVO      PIC X(7).
           10 PI-DRPA0798-PMPR-GEL-RABST     PIC X(5).
           10 PI-DRPA0798-PMPR-GEL-EALTER    PIC S9(4) USAGE COMP.
           10 PI-DRPA0798-PMPR-NBB-SUMME    PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-AUW-SUMME    PIC S9(9)V9(2) USAGE COMP-3.
           10 PI-DRPA0798-PMPR-VERTRAGSTEIL  PIC S9(004) BINARY.
      *
       01 PO-DRPA0798.
          05 PO-DRPA0798-RC                   PIC  X(003).
             88 PO-DRPA0798-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0798-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0798-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
 
 


