      *****************************************************************
      *    COPY-MEMBER: DRKI3942                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3942                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.05.2022 / 09:08:30 / U131774
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
      *    FÜR DIE FUNKTION DRKI3942
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3942.
           05  PI-DRKI3942-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3942-G48PKUKTONR   PIC  X(0002).
             10 PI-DRKI3942-G48PKVNR   PIC  X(0015).
             10 PI-DRKI3942-G48PKWAEHRUNG   PIC  X(0003).
END   *

      *
       01  PO-DRKI3942.
           05  PO-DRKI3942-RC                     PIC  X(003).
               88 PO-DRKI3942-RC-OK               VALUE 'OK '.
               88 PO-DRKI3942-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3942-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3942-G48PKANNUITAET   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3942-G48PKBICBELASTKTO   PIC  X(0011).
             10 PO-DRKI3942-G48PKDARLBTRG   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3942-G48PKDELKZN   PIC  X(0001).
             10 PO-DRKI3942-G48PKFAELLIGDAT   PIC  X(0008).
             10 PO-DRKI3942-G48PKGSMLAUFZEIT   PIC S9(9) COMP.
             10 PO-DRKI3942-G48PKIBANBELASTKTO   PIC  X(0034).
             10 PO-DRKI3942-G48PKIBANPK   PIC  X(0034).
             10 PO-DRKI3942-G48PKKREDITART   PIC  X(0006).
             10 PO-DRKI3942-G48PKKREDITAUSZDAT   PIC  X(0008).
             10 PO-DRKI3942-G48PKKREDITZUSDAT   PIC  X(0008).
             10 PO-DRKI3942-G48PKMANDREF   PIC  X(0035).
             10 PO-DRKI3942-G48PKNAMEBELASTKTO   PIC  X(0028).
             10 PO-DRKI3942-G48PKOEFFDAT   PIC  X(0008).
             10 PO-DRKI3942-G48PKPIN   PIC S9(9) COMP.
             10 PO-DRKI3942-G48PKRESTLAUFZEIT   PIC S9(9) COMP.
             10 PO-DRKI3942-G48PKSALDO   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3942-G48PKSCHLDAT   PIC  X(0008).
             10 PO-DRKI3942-G48PKSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3942-G48PKVERSUMFANGKN1   PIC  X(0001).
             10 PO-DRKI3942-G48PKVERSUMFANGKN2   PIC  X(0001).
             10 PO-DRKI3942-LETZTEAENDERUNG   PIC  X(0026).
             10 PO-DRKI3942-LETZTERAENDERER   PIC  X(0010).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************