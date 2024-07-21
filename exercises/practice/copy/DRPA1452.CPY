      ****************************************************************
      *    COPY-MEMBER: DRPA1452  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Brenner                                         *
      *    DATUM:    05.01.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!05.01.01! 01 ! Neuerstellung             ! Brenner  *
      * APA01539!26.07.13!2.2 ! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      *         !06.08.13!2.3 ! PMPP-AE-KZ ERGÄNZ.KALYPSO ! STRAUSS  *
      * APA01932!31.03.15!2.3 ! ERWEITERUNG FÜR LUCY      ! KÖPRÜCÜ  *
      * APA02220!15.03.16!2.6 ! ERWEITERUNG KONDITIONEN   ! SCHEIDEL *
      * APA02275!30.01.17!2.7 ! ERWEITERUNG FlexN-Felder  ! KÖPRÜCÜ  *
      * APA02353!14.03.17!2.9 ! ERWEITERUNG TELESON       ! BUCH     *
      * FPA01603!04.10.17!3.0 ! SATZLÄNGE KORREKTUR       ! WANG     *
      * APA02579!26.04.18!3.1 ! FrWI neue DB2-Attribute   ! Hofmann  *
      *         !        !    ! Satzlänge erhöht          !          *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    ! NBB-, AUW-SUMME HINZU     !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1452                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1452.
          05 PI-DRPA1452-UFKT               PIC  9(002).
          05 PI-DRPA1452-DATEN              PIC  X(419).
      *
       01 PO-DRPA1452.
          05 PO-DRPA1452-RC                 PIC  X(003).
             88 PO-DRPA1452-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
 
 
 
 



