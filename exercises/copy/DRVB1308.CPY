      ****************************************************************
      *    COPY-MEMBER: DRVB1308 (ERSTELLT FÜR ADM.-CLUB)            *
      *    FUNKTION ERMITTELT FÜR EINEN KANDIDATEN ALLE DIREKT HERAUS*
      *    GEWACHSENEN ED-KONTEN (1 GEN.) LETZTEN 12 MONATE MIT DEN  *  
      *    IM ZEITRAUM ENTSTANDENEN EINHEITEN.                       *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: T13AE30                                         *
      *    DATUM:    08.04.2021                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *
      * -------------------------------------------------------------*
      * VLA-2350!08.04.21! NEUE FUNKTION DRVB1308         ! WILLIG   *
      *         !        !                                !          *  
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1308                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1308.
          05 PI-DRVB1308-PROGRAMM                PIC  X(008).
          05 PI-DRVB1308-UFKT-CODE               PIC  9(002).
          05 PI-DRVB1308-MA-ST-NR                PIC  S9(009)   COMP.
      *
       01 PO-DRVB1308.
          05 PO-DRVB1308-RC                      PIC  X(003).
             88 PO-DRVB1308-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1308-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB1308-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1308-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1308-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1308-DATEN.
             10 PO-DRVB1308-ED-ST-NR             PIC  S9(009)   COMP.
             10 PO-DRVB1308-DAT-VON              PIC   X(010).
             10 PO-DRVB1308-NET_GES_EH           PIC  S9(009)V9 COMP-3.
      *
      ******** Ende Copy-Member DRVB1308 *****************************
