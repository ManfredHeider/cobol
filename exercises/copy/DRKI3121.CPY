      *****************************************************************
      *    COPY-MEMBER: DRKI3121
      *    --------------------------------------------------------
      *    FUNKTION LIST FÜR ZXKI0562
      *    --------------------------------------------------------
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3121
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DBO-10293  !06.09.22! NEU                          ! T13AC20
      * -----------!--------!------------------------------!----------
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
       01 PI-DRKI3121.
          05 PI-DRKI3121-UFKT-CODE            PIC  9(002).
             88 PI-DRKI3121-UFKT-FIRST             VALUE 1.
             88 PI-DRKI3121-UFKT-NEXT              VALUE 2.
             88 PI-DRKI3121-UFKT-QUIT              VALUE 3.
          05 PI-DRKI3121-KEY.
             10 PI-DRKI3121-G21VVMANDANT      PIC S9(009) BINARY.
      *
       01 PO-DRKI3121.
          05 PO-DRKI3121-RC                   PIC  X(003).
             88 PO-DRKI3121-RC-OK                  VALUE 'OK '.
             88 PO-DRKI3121-RC-LEER                VALUE 'LER'.
             88 PO-DRKI3121-RC-EOF                 VALUE 'EOF'.
          05 PO-DRKI3121-DATEN.
             10 PO-DRKI3121-G21VKVNR          PIC  X(015).
             10 PO-DRKI3121-G21VKVERARB-KZN   PIC  X(01).
             10 PO-DRKI3121-G21VKNACHNAME     PIC  X(080).
             10 PO-DRKI3121-G21VKVORNAME      PIC  X(030).
             10 PO-DRKI3121-G21VKGEBDAT       PIC  X(010).
             10 PO-DRKI3121-G21VKANREDE       PIC S9(4) BINARY.
             10 PO-DRKI3121-G21VKGESCHLECHT   PIC  X(001).
             10 PO-DRKI3121-G21VKSTR          PIC  X(045).
             10 PO-DRKI3121-G21VKPLZ          PIC  X(015).
             10 PO-DRKI3121-G21VKORT          PIC  X(030).
             10 PO-DRKI3121-G21VKLKZN         PIC  X(003).
             10 PO-DRKI3121-LETZTERAENDERER   PIC  X(010).
             10 PO-DRKI3121-LETZTEAENDERUNG   PIC  X(026).
      *****************************************************************
      *---------------------- ENDE DRKI3121 --------------------------*
      *****************************************************************