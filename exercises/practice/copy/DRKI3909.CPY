      *****************************************************************
      *    COPY-MEMBER: DRKI3909                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3909                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.10.2021 / 14:00:26 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DBO-8787!11.10.21!    ! Neuerstellung             ! u130685  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3909
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3909.
           05  PI-DRKI3909-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3909-G48BFUKTONR   PIC  X(0002).
             10 PI-DRKI3909-G48BFVNR   PIC  X(0015).
             10 PI-DRKI3909-G48BFWAEHRUNG   PIC  X(0003).
END   *

      *
       01  PO-DRKI3909.
           05  PO-DRKI3909-RC                     PIC  X(003).
               88 PO-DRKI3909-RC-OK               VALUE 'OK '.
               88 PO-DRKI3909-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************