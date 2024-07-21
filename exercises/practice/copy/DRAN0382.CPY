      *****************************************************************
      *    COPY-MEMBER: DRAN0382                                     
      *                                                              
      *   <KURZE FACHLICHE BESCHREIBUNG>                             
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0382
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<                                           *
      *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1090!28.06.21! NEU                          ! RAHNER   *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01  PI-DRAN0382.
           05  PI-DRAN0382-KEY.
             10 PI-DRAN0382-CABA-HVANTNUM       PIC  X(009).
      *
       01  PO-DRAN0382.
           05  PO-DRAN0382-RC                   PIC  X(003).
               88 PO-DRAN0382-RC-OK                  VALUE 'OK '.
               88 PO-DRAN0382-RC-NOT-FOUND           VALUE 'NFD'.
      *****************************************************************
      *---------------------- ENDE DRAN0382 --------------------------* 
      *****************************************************************
