      *****************************************************************
      *    COPY-MEMBER: DRPA1982
      *    --------------------------------------------------------
      *    DIESES COPY ENTHÄLT DIE DATENSATZBESCHREIBUNG DES GES. 72
      *    INFINA PROVISIONS-DTA
      *    --------------------------------------------------------
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1982
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DAISY-5194 !01.02.23! NEU                          ! BUCH
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
           30 INFINA-DATENSATZ.
              35 VNR                        PIC  9(015).
              35 VERTRAG-BEGINN             PIC  X(010).
              35 FAELLIG-VON                PIC  X(010).
              35 AVERM                      PIC  9(007).
              35 PGEB-GES                   PIC  9(002).
              35 KUNDE-NAME                 PIC  X(025).
              35 KUNDE-VNAME                PIC  X(020).
              35 PG-ABR-DAT                 PIC  X(010).
              35 WAEHRUNG                   PIC  X(003).
              35 PG-PGV                     PIC  9(004).
              35 PG-TARIF                   PIC  X(012).
              35 PROV-SATZ                  PIC  9(003)V9(03).
              35 PROV-BETRAG                PIC  9(007)V9(02).
              35 PPFL-SUMME                 PIC  9(007)V9(02).
              35 FILLER                     PIC  X(017).
              
      *****************************************************************
      *---------------------- ENDE DRPA1982 --------------------------* 
      *****************************************************************
