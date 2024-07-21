      ****************************************************************
      *    COPY-MEMBER: DRPA1598  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01539!28.02.14! 2.3! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA02353!30.03.17! 2.4! ERWEITERUNG FÜR Teleson   ! Buch     *
      * APA02579!12.10.18! 2.5! ERWEITERUNG FÜR FriWind   ! Buch     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    ! LAENGE KORRIGIERT         !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1598                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT-QSAM <<<                                      *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1598.
           05 PI-DRPA1598-UFKT         PIC  9(002).
           05 PI-DRPA1598-DATEN        PIC  X(406).
      *
        01 PO-DRPA1598.
           05 PO-DRPA1598-RC           PIC  X(003).
              88 PO-DRPA1598-RC-OK          VALUE 'OK '.
 
      * ******************* ENDE COPY-MEMBER *************************

