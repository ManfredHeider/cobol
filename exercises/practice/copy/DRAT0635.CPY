      *****************************************************************
      *    COPY-MEMBER: DRAT0635                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJAT0003                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    23.09.2021                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * VLA-3405   !23.09.21! NEU                          ! KLEEMEIER*
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRAT0635 - ERMITTELN BAUSTEINE FÜR BRIEF          *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRAT0635.
           05  PI-DRAT0635-DATEN.
               10  PI-DRAT0635-UFKT-CODE         PIC  9(002).
               10  PI-DRAT0635-BRIEF             PIC  X(016).
      *
       01  PO-DRAT0635.
           05  PO-DRAT0635-RC                    PIC  X(003).
               88  PO-DRAT0635-RC-OK                  VALUE 'OK '.
               88  PO-DRAT0635-RC-EOF                 VALUE 'EOF'.
           05  PO-DRAT0635-DATEN.
               10  PO-DRAT0635-POS-BAUST         PIC S9(004) COMP.
               10  PO-DRAT0635-BAUSTEIN          PIC  X(016).
               10  PO-DRAT0635-POS-PARAM         PIC S9(004) COMP.
               10  PO-DRAT0635-PARAMETER         PIC  X(016).
               10  PO-DRAT0635-QUELLE            PIC  X(001).
               10  PO-DRAT0635-IWERT             PIC  X(004).
      ****
      **** ENDE COPY DRAT0635 ****************************************
      ****