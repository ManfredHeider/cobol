      ****************************************************************
      *    COPY-MEMBER: drpa0730  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J.OPRITZ                                        *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0022!22.12.97! 01 ! NEUERSTELLUNG             ! J.OPRITZ *
      * APA00012!09.09.98!  2 ! Länge angepasst           ! Parent   *
      * APA01539!26.07.13! 2.1! ERWEITERUNG F. KALYPSO    ! RAHNER   *
      * APA02353!20.02.17! 2.3! ERWEITERUNG F. Teleson    ! Buch     *
      * APA02579!26.04.18! 2.4! FrWI neue DB2-Attribute   ! Hofmann  *
      *         !        !    ! Satzlänge erhöht          !          *
      * DSY04307!21.12.21!    ! SATZLÄNGE WG. NBB-AUW-SUMM! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    ! LAENGE KORRIGIERT         !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION drpa0730                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0730.
          05 PI-DRPA0730-UFKT               PIC  9(002).
          05 PI-DRPA0730-DATEN              PIC  X(406).

      *
       01 PO-DRPA0730.
          05 PO-DRPA0730-RC                 PIC  X(003).
             88 PO-DRPA0730-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************


