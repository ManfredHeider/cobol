      *****************************************************************
      *    COPY-MEMBER: DRKI3943                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3943                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.05.2022 / 09:08:32 / U131774
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DBO-8788!05.05.22!    ! Neuerstellung             ! u131774  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3943
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3943.
           05  PI-DRKI3943-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI3943-G48PKUKTONR   PIC  X(0002).
             10 PI-DRKI3943-G48PKVNR   PIC  X(0015).
             10 PI-DRKI3943-G48PKWAEHRUNG   PIC  X(0003).
END   *

           05  PI-DRKI3943-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3943-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3943-G48PKANNUITAET   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3943-G48PKBICBELASTKTO   PIC  X(0011).
             10 PI-DRKI3943-G48PKDARLBTRG   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3943-G48PKDELKZN   PIC  X(0001).
             10 PI-DRKI3943-G48PKFAELLIGDAT   PIC  X(0008).
             10 PI-DRKI3943-G48PKGSMLAUFZEIT   PIC S9(9) COMP.
             10 PI-DRKI3943-G48PKIBANBELASTKTO   PIC  X(0034).
             10 PI-DRKI3943-G48PKIBANPK   PIC  X(0034).
             10 PI-DRKI3943-G48PKKREDITART   PIC  X(0006).
             10 PI-DRKI3943-G48PKKREDITAUSZDAT   PIC  X(0008).
             10 PI-DRKI3943-G48PKKREDITZUSDAT   PIC  X(0008).
             10 PI-DRKI3943-G48PKMANDREF   PIC  X(0035).
             10 PI-DRKI3943-G48PKNAMEBELASTKTO   PIC  X(0028).
             10 PI-DRKI3943-G48PKOEFFDAT   PIC  X(0008).
             10 PI-DRKI3943-G48PKPIN   PIC S9(9) COMP.
             10 PI-DRKI3943-G48PKRESTLAUFZEIT   PIC S9(9) COMP.
             10 PI-DRKI3943-G48PKSALDO   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3943-G48PKSCHLDAT   PIC  X(0008).
             10 PI-DRKI3943-G48PKSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3943-G48PKVERSUMFANGKN1   PIC  X(0001).
             10 PI-DRKI3943-G48PKVERSUMFANGKN2   PIC  X(0001).
             10 PI-DRKI3943-LETZTERAENDERER   PIC  X(0010).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3943-NI1.
           05 PI-DRKI3943-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3943-NI7.
           05 PI-DRKI3943-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI3943.
           05  PO-DRKI3943-RC                     PIC  X(003).
               88 PO-DRKI3943-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3943-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************