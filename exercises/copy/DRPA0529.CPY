      ****************************************************************
      *    COPY-MEMBER: DRPA0529  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: T. MUND                                         *
      *    DATUM:    25.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA....!25.09.97! 01 ! Neuerstellung             ! T. MUND  *
      *         !        !    !                           !          *
      * AFPA0012!17.11.97!    ! DATENBEREICH AUF 122      ! WOLFF    *
      *         !        !    ! BYTES GEÄNDERT            !          *
      * APA01539!26.07.13!2.1 ! VERGRÖßERN AUF 234 KALYPSO! RAHNER   *
      * APA01932!30.03.15!2.2 ! VERGRÖßERN AUF 247 LUCY   ! KÖPRÜCÜ  *
      * APA02220!15.03.16!2.4 ! ERWEITERUNG KONDITIONEN   ! SCHEIDEL *
      * APA02275!30.01.17!2.5 ! ERWEITERUNG FlexN-Felder  ! KÖPRÜCÜ  *
      * APA02353!09.03.17!2.7 ! ERWEITERUNG TELESON       ! WANG     *
      * APA02579!26.04.18!2.8 ! FrWI neue DB2-Attribute   ! Hofmann  *
      *         !        !    ! Satzlänge erhöht          !          *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    ! LAENGE KORRIGIERT         !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0529                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0529.
          05 PI-DRPA0529-UFKT               PIC  9(002).

          05 PI-DRPA0529-DATEN              PIC  X(383).
      *
       01 PO-DRPA0529.
          05 PO-DRPA0529-RC                 PIC  X(003).
             88 PO-DRPA0529-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
 
 



