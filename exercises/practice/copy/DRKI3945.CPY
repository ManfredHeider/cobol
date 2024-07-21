      *****************************************************************
      *    COPY-MEMBER: DRKI3945                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3945                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.05.2022 / 09:08:36 / U131774
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      * DBO-8788!05.05.22!    ! Neuerstellung             ! u131774  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3945
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3945.
           05  PI-DRKI3945-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3945-G48PKUKTONR   PIC  X(0002).
             10 PI-DRKI3945-G48PKVNR   PIC  X(0015).
             10 PI-DRKI3945-G48PKWAEHRUNG   PIC  X(0003).
END   *

      *
       01  PO-DRKI3945.
           05  PO-DRKI3945-RC                     PIC  X(003).
               88 PO-DRKI3945-RC-OK               VALUE 'OK '.
               88 PO-DRKI3945-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************