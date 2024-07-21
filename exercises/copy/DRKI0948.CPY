      *****************************************************************
      *    COPY-MEMBER: DRKI0948                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0948                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.11.99/17.31.49
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00080!17.11.99!    ! NEU                       !Peter Pohl*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0646
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  DRKI0948.
           05 INPUT-SATZ.
              10 VERVETRAGSID              PIC   X(013).
              10 VERFRAPERSONENID          PIC   X(013).
              10 Filler                    PIC   X.
              10 VERPARTGESELLSCHID        PIC  S9(004) COMP.
              10 VERVERTRAGSNUMMER         PIC   X(015).
              10 Filler                    PIC   X.

      ******* ENDE COPY-MEMBER **************************************
